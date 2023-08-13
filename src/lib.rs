use byteorder::{LittleEndian, ReadBytesExt};
use error_stack::{IntoReport, Report, ResultExt, report};
use libxml::{
    parser::Parser as XmlParser,
    tree::NodeType,
    readonly::RoNode,
    xpath::Context as XpathContext,
};
use std::{
    ffi::CStr,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

mod error;
use error::{ParseNodeError, ReadFileError};

mod data_block;
use data_block::{ChecksumAlgorithm, CompressionAlgorithm, CompressionLevel};

mod image;
pub use image::Image;

#[derive(Clone, Debug)]
pub struct ReadOptions {
    // read FITSKeyword headers from the XML header
    pub import_fits_keywords: bool,
    // import FITSKeyword headers as XISF <Property> tags with the prefix FITS:
    pub fits_keywords_as_properties: bool,
    // replace NaNs, infinities, and negative zeros with the lower bound for floating-point images
    pub fix_non_finite: bool,
}
impl Default for ReadOptions {
    fn default() -> Self {
        Self {
            import_fits_keywords: true,
            fits_keywords_as_properties: false,
            fix_non_finite: true,
        }
    }
}

// TODO: fp_lower_bound and fp_upper_bound only have 64-bit accuracy, whereas XISF can support 128-bit floats
#[derive(Clone, Debug)]
pub struct WriteOptions {
    // name of the application using this library
    pub creator_application: String,
    // write FITS headers as FITSKeyword elements in the XML header
    pub export_fits_keywords: bool,
    // algorithm used for XISF data block checksum calculations
    pub checksum_alg: Option<ChecksumAlgorithm>,
    // algorithm used to compress XISF data blocks
    pub compression_alg: Option<(CompressionAlgorithm, CompressionLevel)>,
    // lower bound for floating-point pixel samples
    pub fp_lower_bound: f64,
    // upper bound for floating-point pixel samples
    pub fp_upper_bound: f64,
    // data blocks are allocated with block sizes of integer multiples of this value, in bytes
    pub block_alignment_size: u16,
    // max size (in bytes) that an XISF data block can be before it can no longer be inlined/embedded
    // recommended value: 3/4 the size of block_alignment_size (or a multiple of it), since base64 takes 4 chars to encode 3 bytes
    // that is, a maximum-size inline data block can be base64-encoded into a buffer the same size as the block alignment size
    pub max_inline_block_size: u16,
}
impl WriteOptions {
    pub fn new(app_name: impl Into<String>) -> Self {
        Self {
            creator_application: app_name.into(),
            export_fits_keywords: true,
            checksum_alg: Some(ChecksumAlgorithm::Sha1), // * differs from reference implementation, where default is no checksum
            compression_alg: None,
            fp_lower_bound: 0.0,
            fp_upper_bound: 1.0,
            block_alignment_size: 4096,
            max_inline_block_size: 3072, // a block of 3072 bytes takes 4096 bytes in base64 encoding
        }
    }
}

#[derive(Clone, Debug)]
pub struct XISF {
    pub initial_comment: Option<String>,
    pub images: Vec<Image>,
}
impl XISF {
    pub fn read_file(filename: impl AsRef<Path>, opts: &ReadOptions) -> Result<Self, Report<ReadFileError>> {
        let filename_str = filename.as_ref().to_string_lossy().to_string();
        let _span_guard = tracing::debug_span!("read_file", filename = filename_str).entered();

        let f = File::open(filename)
            .into_report()
            .change_context(ReadFileError)
            .attach_printable_lazy(|| format!("Failed to open file {filename_str} for reading"))?;
        let mut reader = BufReader::new(f);

        // verify that the first 8 bytes of the file are XISF0100
        const CORRECT_SIGNATURE: [u8; 8] = *b"XISF0100";
        let mut signature_buf = [0u8; 8];
        reader
            .read_exact(&mut signature_buf)
            .into_report()
            .change_context(ReadFileError)
            .attach_printable("Failed to read 8-byte field \"file format signature\" at start of file")?;
        if signature_buf != CORRECT_SIGNATURE {
            return Err(report!(ReadFileError))
                .attach_printable(format!("8-byte signature at start of file {signature_buf:?} did not match expected {CORRECT_SIGNATURE:?} (XISF0100)"));
        }

        // next 4 bytes are a little-endian encoded unsigned integer specifying the length of the XML header
        let header_length = reader
            .read_u32::<LittleEndian>()
            .into_report()
            .change_context(ReadFileError)
            .attach_printable("Error parsing 4-byte field \"XML header length\" as little-endian u32")?;
        tracing::debug!("Header size: {} bytes", header_length);

        const RESERVED_BYTES: i64 = 4;
        reader
            .seek_relative(RESERVED_BYTES)
            .into_report()
            .change_context(ReadFileError)
            .attach_printable("Failed to skip 4 reserved bytes")?;

        // read header to buffer
        let mut header_buf = vec![0u8; header_length as usize];
        reader
            .read_exact(&mut header_buf)
            .into_report()
            .change_context(ReadFileError)
            .attach_printable_lazy(|| format!("Failed to read {header_length}-byte XML header from file"))?;

        // parse the header
        let xml = XmlParser::default().parse_string(header_buf)
            .into_report()
            .change_context(ReadFileError)
            .attach_printable("Failed to parse XML header")?;

        // TODO: make a version of xmlsec that fits my needs
        // - supports embedded keys, propagates errors instead of panicking, cross-platform
        // // verify signature before reading anything at all
        // let ctx = XmlSecSignatureContext::new();
        // match ctx.verify_document(&xml) {
        //     Ok(true) | Err(XmlSecError::NodeNotFound) => (),
        //     Ok(false) => return Err(report!(ReadFileError))
        //         .attach_printable("XML header failed cryptographic signature verification"),
        //     Err(e) => return Err(report!(e))
        //         .change_context(ReadFileError)
        //         .attach_printable("Error while verifying cryptographic signature of XML header"),
        // }

        // verify xml declaration version and encoding
        let version = unsafe { CStr::from_ptr((*xml.doc_ptr()).version as *const i8) };
        if version.to_bytes() != "1.0".as_bytes() {
            return Err(report!(ReadFileError))
                .attach_printable("XISF spec requires XML version 1.0 in XML declaration")
        }

        let encoding = unsafe { CStr::from_ptr((*xml.doc_ptr()).encoding as *const i8) };
        if encoding.to_bytes() != "UTF-8".as_bytes() {
            return Err(report!(ReadFileError))
                .attach_printable("XISF spec requires UTF-8 encoding in XML declaration")
        }

        // the spec disallows parsers from using the content of this comment to change its behavior,
        // but it doesn't forbid us from making note of it, just in case anyone cares
        let comment = xml.as_node()
            .get_child_nodes()
            .into_iter()
            .filter(|n| n.get_type() == Some(NodeType::CommentNode))
            .next()
            .map(|n| n.get_content());

        let root = xml.get_root_readonly()
            .ok_or(report!(ReadFileError))
            .attach_printable("No root element found in XML header")?;

        // we need to pass down a global xpath context in order to resolve <Reference> elements
        let xpath = XpathContext::from_node(&xml.get_root_element().unwrap())
            .map_err(|_| report!(ReadFileError))
            .attach_printable("Failed to create XPATH context for XML header")?;

        // xisf root element assigns a default namespace, but does not associate a prefix with it
        // in order to select these nodes by name with xpath, we have to assign them a prefix ourselves
        xpath.register_namespace("xisf", "http://www.pixinsight.com/xisf")
            .map_err(|_| report!(ReadFileError))
            .attach_printable("Failed to associate prefix to xisf namespace in XML header")?;

        Self::parse_root_node(root, &xpath, opts)
            .change_context(ReadFileError)
            .map(|xisf| {
                Self { // surgically reinsert the comment for safekeeping
                    initial_comment: comment,
                    ..xisf
                }
            })
    }

    fn parse_root_node(node: RoNode, xpath: &XpathContext, opts: &ReadOptions) -> Result<XISF, Report<ParseNodeError>> {
        const CONTEXT: ParseNodeError = ParseNodeError("xisf");
        if node.get_name() != "xisf" {
            return Err(report!(CONTEXT))
                .attach_printable("Root element in XML header must be named \"xisf\"");
        }

        // * this is mutable because we use .remove() instead of .get()
        // that way, after we've extracted everything we recognize,
        // we can just iterate through what remains and emit warnings
        // saying we don't know what so-and-so attribute means
        let mut attrs = node.get_attributes();

        // do not validate namespace and schema attributes (spec says SHOULD have them, not MUST)
        // version MUST be 1.0, though
        match attrs.remove("version").as_deref() {
            Some("1.0") => (),
            None => return Err(report!(CONTEXT))
                .attach_printable("Missing version attribute for <xisf> element in XML header"),
            Some(bad) => return Err(report!(CONTEXT))
                .attach_printable(format!("Invalid version attribute for <xisf> element in XML header: expected \"1.0\", found \"{bad}\"")),
        }

        // we don't actually care if xsi:schemaLocation exists, or what its value is
        // just calling remove here so it doesn't create a warning below if it exists
        attrs.remove("schemaLocation");

        for remaining in attrs.into_iter() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }

        let mut images = vec![];
        for child in node.get_child_nodes() {
            match child.get_name().as_str() {
                "Image" => images.push(Image::parse_node(child, xpath, opts).change_context(CONTEXT)?),
                _ => tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name()),
            }
        }

        Ok(XISF {
            initial_comment: None,
            images,
        })
    }
}
