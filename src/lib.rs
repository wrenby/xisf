//! [![Crates.io](https://img.shields.io/crates/v/xisf-rs)](https://crates.io/crates/xisf-rs)
//! ![Minimum rustc version](https://img.shields.io/badge/rustc-1.64+-lightgray.svg)
//! ![License](https://img.shields.io/crates/l/xisf-rs.svg)
//!
//! An unaffiliated implementation of Pleiades Astrophoto's open-source Extensible Image Serialization Format (XISF) file format,
//! the native image format for their flagship editing software PixInsight. Aims for 100% support for
//! [spec version 1.0](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html), as opposed to implementations such as
//! [libXISF](https://gitea.nouspiro.space/nou/libXISF) or Pleiades Astrophoto's own [PixInsight Class Libraries](https://gitlab.com/pixinsight/PCL),
//! which are written with 2D images in mind.
//!
//! See the examples folder for a a simple XISF to FITS converter powered by this library.
//! <div class="warning">
//!
//! The examples folder is set up as part of a workspace, which turns off example auto-discovery.
//! The command to run the program in subfolder `examples/NAME` from the root directory
//! is `cargo run -p NAME` rather than `cargo run --example NAME`.
//!
//! </div>

#![warn(missing_debug_implementations, rust_2018_idioms, missing_docs)]

use byteorder::{LittleEndian, ReadBytesExt};
use error_stack::{Report, Result, ResultExt, report};
use libxml::{
    parser::Parser as XmlParser,
    readonly::RoNode,
    xpath::Context as XpathContext,
};
use std::{
    collections::HashMap,
    ffi::CStr,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

pub mod error;
use error::{ParseNodeError, ParseNodeErrorKind::{self, *}, ReadFileError};

pub mod data_block;
use data_block::{ChecksumAlgorithm, CompressionAlgorithm, CompressionLevel, Context};

pub mod image;
use image::Image;

pub mod table;
use table::Table;

mod reference;
pub(crate) use reference::*;

pub mod property;
use property::*;

mod metadata;
use metadata::Metadata;

#[cfg(feature = "f128")]
pub use f128;

/// Flags to alter the behavior of the reader
///
/// # Example
///
/// ```
/// # use xisf_rs::ReadOptions;
/// let opts = ReadOptions::new()
///     .import_fits_keywords(false)
///     .clamp_to_bounds(false);
/// ```
#[derive(Clone, Debug)]
pub struct ReadOptions {
    pub(crate) import_fits_keywords: bool,
    pub(crate) fits_keywords_as_properties: bool,
    pub(crate) clamp_to_bounds: bool,
}
impl ReadOptions {
    /// Alias for [`Default::default()`]
    pub fn new() -> Self {
        Default::default()
    }
    /// Read FITSKeyword headers from the XML header
    pub fn import_fits_keywords(&mut self, import: bool) -> &mut Self {
        self.import_fits_keywords = import;
        self
    }
    /// Import FITSKeyword headers as XISF &lt;Property&gt; tags with the prefix FITS:
    ///
    /// Has no effect if [`Self::import_fits_keywords()`] is false
    ///
    /// <div class="warning">Not currently respected</div>
    pub fn fits_keywords_as_properties(&mut self, convert: bool) -> &mut Self {
        self.fits_keywords_as_properties = convert;
        self
    }
    /// Clamp all pixel samples to the range specified in the bounds attribute
    ///
    /// For floating-point images: NaNs, infinities, and negative zeros are replaced with the lower bound
    ///
    /// <div class="warning">Not currently respected</div>
    pub fn clamp_to_bounds(&mut self, clamp: bool) -> &mut Self {
        self.clamp_to_bounds = clamp;
        self
    }
}
impl Default for ReadOptions {
    fn default() -> Self {
        Self {
            import_fits_keywords: true,
            fits_keywords_as_properties: false,
            clamp_to_bounds: true,
        }
    }
}


/// Flags to alter the behavior of the writer
///
/// # Example
///
/// ```
/// # use xisf_rs::WriteOptions;
/// # use xisf_rs::data_block::{ChecksumAlgorithm, CompressionAlgorithm, CompressionLevel};
/// let opts = WriteOptions::new("My Awesome Astronomy Program")
///     .checksum_algorithm(Some(ChecksumAlgorithm::Sha3_512))
///     .compression_algorithm(Some((CompressionAlgorithm::Zlib, CompressionLevel::AUTO)));
/// ```
#[derive(Clone, Debug)]
pub struct WriteOptions {
    /// Name of the application using this library
    pub(crate) creator_application: String,
    /// Write FITS headers as FITSKeyword elements in the XML header
    pub(crate) export_fits_keywords: bool,
    /// Algorithm used for XISF data block checksum calculations
    pub(crate) checksum_alg: Option<ChecksumAlgorithm>,
    /// Algorithm used to compress XISF data blocks
    pub(crate) compression_alg: Option<(CompressionAlgorithm, CompressionLevel)>,
    /// Lower bound for floating-point pixel samples
    pub(crate) fp_lower_bound: f64,
    /// Upper bound for floating-point pixel samples
    pub(crate) fp_upper_bound: f64,
    /// Data blocks are allocated with block sizes of integer multiples of this value, in bytes
    pub(crate) block_alignment_size: u16,
    /// Max size (in bytes) that an XISF data block can be before it can no longer be inlined/embedded.
    /// Recommended value: 3/4 the size of block_alignment_size (or a multiple of it), since base64 takes 4 chars to encode 3 bytes.
    /// That is, a maximum-size inline data block can be base64-encoded into a buffer the same size as the block alignment size
    pub(crate) max_inline_block_size: u16,
}
impl WriteOptions {
    /// Creates a new `WriteOptions` with the given app name,
    /// and all other options set to their default values
    pub fn new(app_name: impl Into<String>) -> Self {
        Self {
            creator_application: app_name.into(),
            export_fits_keywords: true,
            checksum_alg: None,
            compression_alg: None,
            fp_lower_bound: 0.0,
            fp_upper_bound: 1.0,
            block_alignment_size: 4096,
            max_inline_block_size: 3072, // a block of 3072 bytes takes 4096 bytes in base64 encoding
        }
    }

    /// Name of the application using this library
    pub fn app_name(&mut self, name: String) -> &mut Self {
        self.creator_application = name;
        self
    }
    /// Write FITS headers as FITSKeyword elements in the XML header
    pub fn export_fits_keywords(&mut self, export: bool) -> &mut Self {
        self.export_fits_keywords = export;
        self
    }
    /// Algorithm used for XISF data block checksum calculations
    pub fn checksum_algorithm(&mut self, alg: Option<ChecksumAlgorithm>) -> &mut Self {
        self.checksum_alg = alg;
        self
    }
    /// Algorithm used to compress XISF data blocks
    pub fn compression_algorithm(&mut self, alg: Option<(CompressionAlgorithm, CompressionLevel)>) -> &mut Self {
        self.compression_alg = alg;
        self
    }
    /// Lower bound for floating-point pixel samples
    pub fn fp_lower_bound(&mut self, low: f64) -> &mut Self {
        self.fp_lower_bound = low;
        self
    }
    /// Upper bound for floating-point pixel samples
    pub fn fp_upper_bound(&mut self, high: f64) -> &mut Self {
        self.fp_upper_bound = high;
        self
    }
    /// Data blocks are allocated with block sizes of integer multiples of this value, in bytes
    pub fn block_alignment_size(&mut self, size: u16) -> &mut Self {
        self.block_alignment_size = size;
        self
    }
    /// Max size (in bytes) that an XISF data block can be before it can no longer be inlined/embedded.
    /// Recommended value: 3/4 the size of block_alignment_size (or a multiple of it), since base64 takes 4 chars to encode 3 bytes.
    /// That is, a maximum-size inline data block can be base64-encoded into a buffer the same size as the block alignment size
    pub fn max_inline_block_size(&mut self, size: u16) -> &mut Self {
        self.max_inline_block_size = size;
        self
    }
}

fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}
const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("xisf", kind)
}

/// An XISF file
///
/// Not limited to monolithic files (that is, XISH files are also supported)
#[derive(Clone, Debug)]
pub struct XISF {
    images: Vec<Image>,
    properties: Properties,
    tables: Vec<Table>,
    metadata: Metadata,
}
impl XISF {
    /// Opens a file from disk
    pub fn open(filename: impl AsRef<Path>, opts: &ReadOptions) -> Result<(Self, Context), ReadFileError> {
        let filename_path = filename.as_ref();
        let filename_str = filename_path.to_string_lossy().to_string();
        let _span_guard = tracing::debug_span!("open", filename = filename_str).entered();

        let f = File::open(filename_path)
            .change_context(ReadFileError)
            .attach_printable_lazy(|| format!("Failed to open file {filename_str} for reading"))?;
        let mut reader = BufReader::new(f);

        let extension = filename_path.extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext.to_lowercase());

        let mut header_buf;
        let ctx;
        if let Some("xisf") = extension.as_deref() {
            // verify that the first 8 bytes of the file are XISF0100
            const CORRECT_SIGNATURE: [u8; 8] = *b"XISF0100";
            let mut signature_buf = [0u8; 8];
            reader
                .read_exact(&mut signature_buf)
                .change_context(ReadFileError)
                .attach_printable("Failed to read 8-byte field \"file format signature\" at start of file")?;
            if signature_buf != CORRECT_SIGNATURE {
                return Err(report!(ReadFileError))
                    .attach_printable(format!("Illegal file format signature: expected {CORRECT_SIGNATURE:?} (XISF0100), found {signature_buf:?}"));
            }

            // next 4 bytes are a little-endian encoded unsigned integer specifying the length of the XML header
            let header_length = reader
                .read_u32::<LittleEndian>()
                .change_context(ReadFileError)
                .attach_printable("Error parsing 4-byte field \"XML header length\" as little-endian u32")?;
            tracing::debug!("Header size: {} bytes", header_length);

            const RESERVED_BYTES: i64 = 4;
            reader
                .seek_relative(RESERVED_BYTES)
                .change_context(ReadFileError)
                .attach_printable("Failed to skip 4 reserved bytes")?;

            // read header to buffer
            header_buf = vec![0u8; header_length as usize];
            reader
                .read_exact(&mut header_buf)
                .change_context(ReadFileError)
                .attach_printable_lazy(|| format!("Failed to read {header_length}-byte XML header from file"))?;
            ctx = Context::monolithic(reader);
        } else if let Some("xish") = extension.as_deref() {
            header_buf = vec![];
            reader.read_to_end(&mut header_buf)
                .change_context(ReadFileError)
                .attach_printable("Failed to read XML header from XISH file")?;

            // this unwrap is safe because:
            // 1. when called on "dir/file.ext", returns Some("dir")
            // 2. when called on "file.ext", returns Some("")
            // 3. we know at minimum that the path contains a file because we just opened it
            ctx = Context::distributed(filename_path.parent().unwrap().to_owned());
        } else if let Some(bad) = extension {
            return Err(report!(ReadFileError))
                .attach_printable(format!("Unsupported file extension: {bad}"))
        } else {
            return Err(report!(ReadFileError))
                .attach_printable("File must have an extension to be able to distinguish XISF files from XISH files")
        };

        // parse the header
        let xml = XmlParser::default().parse_string(header_buf)
            .change_context(ReadFileError)
            .attach_printable("Failed to parse XML header")?;

        // TODO: make an interface for xmlsec that fits my needs
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

        let root = xml.get_root_readonly()
            .ok_or(report!(ReadFileError))
            .attach_printable("No root element found in XML header")?;

        // we need to pass down a global xpath context in order to resolve <Reference> elements
        let xpath = XpathContext::new(&xml)
            .map_err(|_| report!(ReadFileError))
            .attach_printable("Failed to create XPATH context for XML header")?;

        // xisf root element should have a default namespace, but does not associate a prefix with it
        // in order to select these nodes by name with xpath, we have to assign them a prefix ourselves
        // ! spec doesn't require this namespace to exist -- how to handle?
        // frankly I don't know see the point of having the namespace is if you don't make it mandatory
        xpath.register_namespace("xisf", "http://www.pixinsight.com/xisf")
            .map_err(|_| report!(ReadFileError))
            .attach_printable("Failed to associate prefix to xisf namespace in XML header")?;

        if root.get_name() != "xisf" {
            return Err(report!(ReadFileError))
                .attach_printable("Root element in XML header must be named \"xisf\"");
        } else {
            Ok((
                Self::parse_root_node(root, &xpath, opts)
                    .change_context(ReadFileError)?,
                ctx
            ))

        }
    }

    fn parse_root_node(node: RoNode, xpath: &XpathContext, opts: &ReadOptions) -> Result<XISF, ParseNodeError> {

        // * this is mutable because we use .remove() instead of .get()
        // that way, after we've extracted everything we recognize,
        // we can just iterate through what remains and emit warnings
        // saying we don't know what so-and-so attribute means
        let mut attrs = node.get_attributes();

        // do not validate namespace and schema attributes (spec says SHOULD have them, not MUST)
        // version MUST be 1.0, though
        match attrs.remove("version").as_deref() {
            Some("1.0") => (),
            None => return Err(report(MissingAttr))
                .attach_printable("Missing version attribute for <xisf> element in XML header"),
            Some(bad) => return Err(report(InvalidAttr))
                .attach_printable(format!("Invalid version attribute for <xisf> element in XML header: expected \"1.0\", found \"{bad}\"")),
        }

        // we don't actually care if xsi:schemaLocation exists, or what its value is
        // just calling remove here so it doesn't create a warning below if it exists
        attrs.remove("schemaLocation");

        for remaining in attrs.into_iter() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }

        let mut images = vec![];
        let mut tables = vec![];
        let mut properties = HashMap::new();
        let mut metadata = None;
        for mut child in node.get_child_nodes() {
            child = child.follow_reference(xpath).change_context(context(InvalidReference))?;
            match child.get_name().as_str() {
                "Image" => images.push(Image::parse_node(child, xpath, opts)?),
                "Property" => {
                    let prop = Property::parse_node(child)?;
                    if properties.insert(prop.id.clone(), prop.content).is_some() {
                        tracing::warn!("Duplicate property found with id {} -- discarding the previous one", prop.id);
                    }
                },
                "Table" => tables.push(Table::parse_node(child, xpath, opts)?),
                "Metadata" => {
                    if metadata.replace(Metadata::parse_node(child, xpath)?).is_some() {
                        tracing::warn!("Duplicate Metadata element found -- discarding the previous one");
                    }
                }
                // TODO: check if the unrecognized node has a uid tag with a reference to it somewhere before emitting a warning
                _ => tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name()),
            }
        }
        let metadata = metadata
            .ok_or(report(MissingChild))
            .attach_printable("Missing Metadata element")?;

        Ok(XISF {
            images,
            properties: Properties::new(properties),
            tables,
            metadata,
        })
    }

    /// Returns an iterator over all images in the file
    pub fn images(&self) -> impl Iterator<Item = &Image> {
        self.images.iter()
    }
    /// Returns the total number of images in the file
    ///
    /// <div class="warning">
    ///
    /// Although XISF is colloquially an image format, the XISF spec allows for files with no images at all.
    /// Such files may instead have arbitrary data stored in tables, or root-level XISF properties.
    ///
    /// </div>
    pub fn num_images(&self) -> usize {
        self.images.len()
    }
    /// Returns a reference to the `i`-th image in the file
    ///
    /// # Panics
    /// If `i` is outside the range `0..num_images()`
    pub fn image(&self, i: usize) -> &Image {
        &self.images[i]
    }

    /// Returns a collection of all properties which were applied directly to the XISF root element (this is uncommon, but allowed)
    ///
    /// Images have their own set of properties that will not be included in this list
    pub fn properties(&self) -> &Properties {
        &self.properties
    }

    /// Returns an iterator over all tables associated with the root element
    pub fn tables(&self) -> impl Iterator<Item = &Table> {
        self.tables.iter()
    }
    /// Returns the total number of tables associated with the root element
    pub fn num_tables(&self) -> usize {
        self.tables.len()
    }
    /// Returns a reference to the `i`-th table associated with the root element
    ///
    /// # Panics
    /// If `i` is outside the range `0..num_tables()`
    pub fn table(&self, i: usize) -> &Table {
        &self.tables[i]
    }

    /// Returns a collection of all [metadata properties](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__XISF_Core_Elements_:_Metadata_Core_Element_:_Mandatory_Metadata_Properties__)
    /// present in the file
    // TODO: distinguish properties() and metadata()
    pub fn metadata(&self) -> &Properties {
        &self.metadata.0
    }
}
