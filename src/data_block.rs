use std::{
    cell::RefCell,
    collections::HashMap,
    io::{Read, BufReader, Seek, Cursor, Take},
    fmt,
    fs::File,
    num::NonZeroU8,
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};

use error_stack::{Report, IntoReport, ResultExt, report};
use flate2::read::ZlibDecoder;
use libxml::{readonly::RoNode, tree::NodeType, xpath::Context as XpathContext};
use parse_int::parse as parse_auto_radix;
use strum::{EnumString, Display, EnumVariantNames};
use url::Url;
use crate::error::{ParseValueError, ParseNodeError, ReadDataBlockError};


#[derive(Debug, Clone, PartialEq)]
pub struct DataBlock {
    pub location: DataBlockReference,
    pub byte_order: ByteOrder,
    pub checksum: Option<Checksum>,
    pub compression: Option<Compression>,
    pub sub_blocks: SubBlocks, // wrapper for Vec<(usize, usize)>, empty vec indicates compression is not using sub-blocks
}
impl DataBlock {
    // returns Ok(Some(_)) if a data block was successfully parsed
    // returns Ok(None) if there is no data block to parse
    // returns Err(_) if there was an error parsing the data block
    // passing &mut attrs isn't for the benefit of this function, but the caller function
    // (helps cut down on unnecessary "ignoring unrecognized attribute" warnings)
    pub(crate) fn parse_node(node: RoNode, xpath: &XpathContext, context: ParseNodeError, attrs: &mut HashMap<String, String>) -> Result<Option<Self>, Report<ParseNodeError>> {
        // TODO: move the code to grab the data from inline and embedded over to here, and remove DataBlockLocation::parse_node in favor of a more standard FromStr implementation
        if let Some(location) = DataBlockReference::parse_node(node, xpath, context, attrs)? {
            let byte_order = match attrs.remove("byteOrder") {
                Some(byte_order) => {
                    byte_order.parse::<ByteOrder>()
                        .into_report()
                        .change_context(context)
                        .attach_printable_lazy(|| format!("Invalid byteOrder attribute: expected one of [big, little], found {byte_order}"))?
                },
                None => Default::default(),
            };

            let checksum = match attrs.remove("checksum") {
                Some(checksum) => Some(
                    checksum.parse::<Checksum>()
                        .change_context(context)
                        .attach_printable("Invalid checksum attribute")?
                ),
                None => None,
            };

            let compression = match attrs.remove("compression") {
                Some(compression) => Some(
                    compression.parse::<Compression>()
                        .change_context(context)
                        .attach_printable("Invalid compression attribute")?
                ),
                None => None,
            };

            let sub_blocks = match attrs.remove("subblocks") {
                Some(compression) => {
                    compression.parse::<SubBlocks>()
                        .change_context(context)
                        .attach_printable("Invalid subblocks attribute")?
                },
                None => SubBlocks(vec![]),
            };

            Ok(Some(DataBlock {
                location,
                byte_order,
                checksum,
                compression,
                sub_blocks,
            }))
        } else {
            Ok(None)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DataBlockReference {
    Plaintext {
        // inline or embedded: data is encoded in a child text or <Data> node
        encoding: Encoding,
        text: String, // stripped of all whitespace
    },
    Attachment {
        // data is elsewhere in the file
        // ! only supported for monolithic XISF files
        position: u64,
        size: u64,
    },
    Url {
        // data is stored remotely
        // ! only supported for distributed XISF files
        url: Url,
        index_id: Option<u64>,
    },
    Path {
        // data is store elsewhere on the filesystem
        // ! only supported for distributed XISF files
        path: PathBuf,
        index_id: Option<u64>,
    }
}
impl DataBlockReference {
    // returns Ok(Some(_)) if a data block location was successfully parsed
    // returns Ok(None) if there is no location attribute
    // returns Err(_) if there was an error parsing the data block location
    // passing &mut attrs isn't for the benefit of this function, but the caller function
    // (helps cut down on unnecessary "ignoring unrecognized attribute" warnings)
    pub(crate) fn parse_node(node: RoNode, _xpath: &XpathContext, context: ParseNodeError, attrs: &mut HashMap<String, String>) -> Result<Option<Self>, Report<ParseNodeError>> {
        if let Some(attr) = attrs.remove("location") {
            match attr.split(":").collect::<Vec<_>>().as_slice() {
                &["inline", encoding] => {
                    let encoding = encoding.parse::<Encoding>()
                        .into_report()
                        .change_context(context)
                        .attach_printable("Invalid location attribute: failed to parse inline encoding")?;

                    match node.get_child_nodes().as_slice() {
                        [] => Err(report!(context)).attach_printable("Missing child text node: required for inline data blocks"),
                        [text] if text.get_type() == Some(NodeType::TextNode) => {
                            let mut text = text.get_content();
                            text.retain(|c| !c.is_whitespace());
                            Ok(Some(
                                Self::Plaintext {
                                    encoding,
                                    text,
                                }
                            ))
                        },
                        _other => Err(report!(context)).attach_printable("XISF Elements with inline data blocks are not permitted to have non-text child nodes"),
                    }
                },
                &["embedded"] =>  {
                    match node.get_child_nodes()
                        .into_iter()
                        .filter(|n| n.get_name() == "Data")
                        .collect::<Vec<_>>()
                        .as_slice()
                    {
                        [] => Err(report!(context)).attach_printable("Missing embedded <Data> node: required for embedded data block location"),
                        [one] => {
                            if let Some(encoding) = one.get_attribute("encoding") {
                                let encoding = encoding.parse::<Encoding>()
                                    .into_report()
                                    .change_context(context)
                                    .attach_printable("Invalid encoding attribute in embedded <Data> node")?;

                                match one.get_child_nodes().as_slice() {
                                    [] => Err(report!(context)).attach_printable("Embedded <Data> node missing child text node"),
                                    [text] if text.get_type() == Some(NodeType::TextNode) => {
                                        let mut text = text.get_content();
                                        text.retain(|c| !c.is_whitespace());
                                        Ok(Some(
                                            Self::Plaintext {
                                                encoding,
                                                text,
                                            }
                                        ))
                                    },
                                    _other => Err(report!(context)).attach_printable("Embedded <Data> nodes are not permitted to have non-text child nodes"),
                                }
                            } else {
                                Err(report!(context)).attach_printable("Embedded <Data> node missing encoding attribute")
                            }
                        },
                        _many => Err(report!(context)).attach_printable("Found more than one embedded <Data> node"),
                    }
                },
                &["attachment", position, size] => {
                    Ok(Some(Self::Attachment {
                        position: parse_auto_radix::<u64>(position.trim())
                            .into_report()
                            .change_context(context)
                            .attach_printable("Invalid location attribute: failed to parse position of attached data block")?,
                        size: parse_auto_radix::<u64>(size.trim())
                            .into_report()
                            .change_context(context)
                            .attach_printable("Invalid location attribute: failed to parse size of attached data block")?,
                    }))
                },
                &[url] if url.starts_with("url(") && url.ends_with(")") => {
                    // parentheses in url must be encoded with XML character references #&40; and &#41;,
                    // but libxml handles that for us transparently
                    Ok(Some(Self::Url {
                        // the slice indexing trims "url(" from the front and ")" from the end
                        url: Url::parse(&url[4..url.len()-1])
                            .into_report()
                            .change_context(context)
                            .attach_printable("Invalid location attribute: failed to parse URL of external data block")?,
                        index_id: None,
                    }))
                },
                &[url, index_id] if url.starts_with("url(") && url.ends_with(")") => {
                    // parentheses in url must be encoded with XML character references #&40; and &#41;,
                    // but libxml handles that for us transparently
                    Ok(Some(Self::Url {
                        // the slice indexing trims "url(" from the front and ")" from the end
                        url: Url::parse(&url[4..url.len()-1])
                            .into_report()
                            .change_context(context)
                            .attach_printable("Invalid location attribute: failed to parse URL of external data block")?,
                        index_id: Some(parse_auto_radix::<u64>(index_id.trim())
                            .into_report()
                            .change_context(context)
                            .attach_printable("Invalid location attribute: failed to parse index-id of external data block")?),
                    }))
                },
                &[path] if path.starts_with("path(") && path.ends_with(")") => {
                    // parentheses in path must be encoded with XML character references #&40; and &#41;,
                    // but libxml handles that for us transparently
                    Ok(Some(Self::Path {
                        // the slice indexing trims "path(" from the front and ")" from the end
                        path: PathBuf::from(&path[5..path.len()-1]),
                        index_id: None,
                    }))
                },
                &[path, index_id] if path.starts_with("path(") && path.ends_with(")") => {
                    // parentheses in path must be encoded with XML character references #&40; and &#41;,
                    // but libxml handles that for us transparently
                    Ok(Some(Self::Path {
                        // the slice indexing trims "path(" from the front and ")" from the end
                        path: PathBuf::from(&path[5..path.len()-1]),
                        index_id: Some(parse_auto_radix::<u64>(index_id.trim())
                            .into_report()
                            .change_context(context)
                            .attach_printable("Invalid location attribute: failed to parse index-id of external data block")?),
                    }))
                },
                _bad => Err(report!(context)).attach_printable("Invalid location attribute: unrecognized pattern")
                    .attach_printable(format!("Expected one of [inline:encoding, embedded, attachment:position:size, url(...), url(...):index-id, path(...), path(...):index-id], found {attr}"))
            }
        } else {
            Ok(None)
        }
    }

    // literally just a byte stream, with no knowledge of compression, byte shuffling, or checksums
    pub(crate) fn raw_bytes(&self, xisf: &crate::XISF) -> Result<Box<dyn Read>, Report<ReadDataBlockError>> {
        let base64 = base64_simd::STANDARD;
        match self {
            Self::Plaintext { encoding, text } => {
                let buf = match encoding {
                    Encoding::Hex => hex_simd::decode_to_vec(text)
                        .into_report()
                        .change_context(ReadDataBlockError)?,
                    Encoding::Base64 => base64.decode_to_vec(text)
                        .into_report()
                        .change_context(ReadDataBlockError)?,
                };
                Ok(Box::new(Cursor::new(buf)))
            },
            Self::Attachment { position, size } => {
                let mut file = File::open(&xisf.filename)
                    .into_report()
                    .change_context(ReadDataBlockError)?;
                file.seek(std::io::SeekFrom::Start(*position))
                    .into_report()
                    .change_context(ReadDataBlockError)?;
                Ok(Box::new(BufReader::new(file).take(*size)))
            },
            #[allow(unused_variables)]
            Self::Url { url, index_id } => {
                todo!()
            },
            #[allow(unused_variables)]
            Self::Path { path, index_id } => {
                todo!()
            },
        }
    }

    // ! if byte shuffling was enabled, this byte stream will still be shuffled
    // - the concept for the data block read functions is a stream, and byte shuffling requires the whole file to be in memory
    pub(crate) fn decompressed_bytes(&self, xisf: &crate::XISF, compression: &Option<Compression>) -> Result<Box<dyn Read>, Report<ReadDataBlockError>> {
        let raw = self.raw_bytes(xisf)?;
        if let Some(compression) = compression {
            match compression {
                Compression::Zlib(..) | Compression::ZlibByteShuffling(..) => {
                    Ok(Box::new(ZlibDecoder::new(raw)))
                },
                Compression::Lz4(..) | Compression::Lz4ByteShuffling(..) | Compression::Lz4HC(..) | Compression::Lz4HCByteShuffling(..) => {
                    Ok(Box::new(
                        lz4::Decoder::new(raw)
                            .into_report()
                            .change_context(ReadDataBlockError)
                            .attach_printable("Failed to open Lz4 stream")?
                    ))
                }
            }
        } else {
            Ok(raw)
        }
    }

    pub fn is_remote(&self) -> bool {
        match self {
            Self::Url { url, .. } if url.scheme() != "file" => true,
            _ => false,
        }
    }
}

/// Allows a reader to be split at arbitrary byte offsets, used for streaming sub-block decompression
/// Intended usage: `reader.multi_take(...).map(|| ...).collect()`
/// But that doesn't work, because correct functioning relies on read_until_empty() after each successive next()
/// * TODO: new idea -- create a `map()` function on `MultiTake<T>`,
/// * and have the result of *that* implement `Read` in a similar fashion to [`std::io::Chain`](https://doc.rust-lang.org/std/io/struct.Chain.html#impl-Read-for-Chain%3CT,+U%3E)
/// * or I don't even really need the middle step, I guess, could just make it a `ReadMapSubBlocksExt`
trait ReadMultiTakeExt {
    type Inner;
    fn multi_take(self, sizes: &[u64]) -> MultiTake<Self::Inner>;
}
impl<R: Read> ReadMultiTakeExt for R {
    type Inner = R;
    fn multi_take(self, sizes: &[u64]) -> MultiTake<R> {
        MultiTake::new(self, sizes)
    }
}
struct MultiTake<T> {
    inner: Arc<RefCell<Take<T>>>,
    sizes: Vec<u64>,
    chunk_index: usize,
}
impl<R: Read> MultiTake<R> {
    /// Panics if `sizes` is empty
    fn new(from: R, sizes: &[u64]) -> MultiTake<R> {
        Self {
            inner: Arc::new(RefCell::new(from.take(sizes[0]))),
            sizes: sizes.to_vec(),
            chunk_index: 0,
        }
    }
}
impl<R: Read> Iterator for MultiTake<R> {
    type Item = Arc<RefCell<Take<R>>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.borrow_mut().set_limit(self.sizes[self.chunk_index]);
        self.chunk_index += 1;
        Some(self.inner.clone())
    }
}

#[derive(Clone, Copy, Debug, Default, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum Encoding {
    #[default]
    #[strum(serialize = "base64")]
    Base64,
    #[strum(serialize = "hex")]
    Hex, // hexadecimal (base 16), must be serialized with a-f in lowercase
}

#[derive(Clone, Copy, Debug, Default, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum ByteOrder {
    #[strum(serialize = "big")]
    Big,
    #[default]
    #[strum(serialize = "little")]
    Little,
}

#[derive(Clone, Copy, Debug, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum ChecksumAlgorithm {
    #[strum(serialize = "sha-1", serialize = "sha1")]
    Sha1,
    #[strum(serialize = "sha-256", serialize = "sha256")]
    Sha256,
    #[strum(serialize = "sha-512", serialize = "sha512")]
    Sha512,
    #[strum(serialize = "sha3-256")]
    Sha3_256,
    #[strum(serialize = "sha3-512")]
    Sha3_512,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Checksum {
    Sha1([u8; 20]),
    Sha256([u8; 32]),
    Sha512([u8; 64]),
    Sha3_256([u8; 32]),
    Sha3_512([u8; 64]),
}
impl fmt::Display for Checksum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn to_hex(digest: &[u8]) -> String {
            hex_simd::encode_to_string(digest, hex_simd::AsciiCase::Lower)
        }
        match &self {
            Self::Sha1(digest) => f.write_fmt(format_args!("sha-1:{}", to_hex(digest))),
            Self::Sha256(digest) => f.write_fmt(format_args!("sha-256:{}", to_hex(digest))),
            Self::Sha512(digest) => f.write_fmt(format_args!("sha-512:{}", to_hex(digest))),
            Self::Sha3_256(digest) => f.write_fmt(format_args!("sha3-256:{}", to_hex(digest))),
            Self::Sha3_512(digest) => f.write_fmt(format_args!("sha3-512:{}", to_hex(digest))),
        }
    }
}
impl FromStr for Checksum {
    type Err = Report<ParseValueError>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        const CONTEXT: ParseValueError = ParseValueError("Checksum");

        fn from_hex(digest: &str, out: &mut [u8]) -> Result<(), Report<ParseValueError>> {
            use hex_simd::AsOut;
            // the comment on this function says it panics if the dest buffer is not large enough,
            // but this is not true -- it returns an Err
            hex_simd::decode(digest.as_bytes(), out[..].as_out())
                .map(|_| ())
                .into_report()
                .change_context(CONTEXT)
                .attach_printable("Failed to decode checksum digest from hexadecimal")
        }

        match s.split_once(":") {
            Some(("sha-1" | "sha1", hex_digest)) => {
                let mut buf = [0u8; 20];
                from_hex(hex_digest, &mut buf[..])?;
                Ok(Self::Sha1(buf))
            },
            Some(("sha-256" | "sha256", hex_digest)) => {
                let mut buf = [0u8; 32];
                from_hex(hex_digest, &mut buf[..])?;
                Ok(Self::Sha256(buf))
            },
            Some(("sha-512" | "sha512", hex_digest)) => {
                let mut buf = [0u8; 64];
                from_hex(hex_digest, &mut buf[..])?;
                Ok(Self::Sha512(buf))
            },
            Some(("sha3-256", hex_digest)) => {
                let mut buf = [0u8; 32];
                from_hex(hex_digest, &mut buf[..])?;
                Ok(Self::Sha3_256(buf))
            },
            Some(("sha3-512", hex_digest)) => {
                let mut buf = [0u8; 64];
                from_hex(hex_digest, &mut buf[..])?;
                Ok(Self::Sha3_512(buf))
            },
            _bad => Err(report!(CONTEXT))
                .attach_printable(format!("Unrecognized pattern: expected checksum-algorithm:hex-digest, found {s}"))
                .attach_printable("Supported checksum algorithms: sha-1, sha-256, sha-512, sha3-256, sha3-512")
        }
    }
}
impl Checksum {
    pub fn as_slice(&self) -> &[u8] {
        match self {
            Checksum::Sha1(digest) => &digest[..],
            Checksum::Sha256(digest) => &digest[..],
            Checksum::Sha512(digest) => &digest[..],
            Checksum::Sha3_256(digest) => &digest[..],
            Checksum::Sha3_512(digest) => &digest[..],
        }
    }
}

#[derive(Clone, Copy, Debug, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum CompressionAlgorithm {
    #[strum(serialize = "zlib")]
    Zlib,
    #[strum(serialize = "zlib+sh")]
    ZlibByteShuffling,
    #[strum(serialize = "lz4")]
    Lz4,
    #[strum(serialize = "lz4+sh")]
    Lz4ByteShuffling,
    #[strum(serialize = "lz4hc")]
    Lz4HC,
    #[strum(serialize = "lz4hc+sh")]
    Lz4HCByteShuffling,
}
impl CompressionAlgorithm {
    pub fn is_shuffled(&self) -> bool {
        match self {
            Self::Zlib | Self::Lz4 | Self::Lz4HC => false,
            Self::ZlibByteShuffling | Self::Lz4ByteShuffling | Self::Lz4HCByteShuffling => true,
        }
    }
}

// TODO: integrate more closely with subblocks
#[derive(Clone, Debug, PartialEq)]
pub enum Compression {
    Zlib(usize),
    ZlibByteShuffling(usize, usize),
    Lz4(usize),
    Lz4ByteShuffling (usize, usize),
    Lz4HC(usize),
    Lz4HCByteShuffling(usize, usize),
}
impl Compression {
    pub fn uncompressed_size(&self) -> usize {
        match self {
            &Compression::Zlib(size) => size,
            &Compression::ZlibByteShuffling(size, _) => size,
            &Compression::Lz4(size) => size,
            &Compression::Lz4ByteShuffling(size, _) => size,
            &Compression::Lz4HC(size) => size,
            &Compression::Lz4HCByteShuffling(size, _) => size,
        }
    }
    pub fn is_shuffled(&self) -> bool {
        match self {
            Self::Zlib(_) | Self::Lz4(_) | Self::Lz4HC(_) => false,
            Self::ZlibByteShuffling(..) | Self::Lz4ByteShuffling(..) | Self::Lz4HCByteShuffling(..) => true,
        }
    }
    pub fn shuffle_item_size(&self) -> Option<usize> {
        match self {
            Compression::Zlib(_) | Compression::Lz4(_) | Compression::Lz4HC(_) => None,
            &Compression::ZlibByteShuffling(_, item_size) => Some(item_size),
            &Compression::Lz4ByteShuffling(_, item_size) => Some(item_size),
            &Compression::Lz4HCByteShuffling(_, item_size) => Some(item_size),
        }
    }
}
impl fmt::Display for Compression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Zlib(uncompressed_size) =>
                f.write_fmt(format_args!("zlib:{uncompressed_size}")),
            Self::ZlibByteShuffling(uncompressed_size, item_size) =>
                f.write_fmt(format_args!("zlib+sh:{uncompressed_size}:{item_size}")),
            Self::Lz4(uncompressed_size) =>
                f.write_fmt(format_args!("lz4:{uncompressed_size}")),
            Self::Lz4ByteShuffling(uncompressed_size, item_size) =>
                f.write_fmt(format_args!("lz4+sh:{uncompressed_size}:{item_size}")),
            Self::Lz4HC(uncompressed_size) =>
                f.write_fmt(format_args!("lz4hc:{uncompressed_size}")),
            Self::Lz4HCByteShuffling(uncompressed_size, item_size) =>
                f.write_fmt(format_args!("lz4hc+sh:{uncompressed_size}:{item_size}")),
        }
    }
}
impl FromStr for Compression {
    type Err = Report<ParseValueError>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        const CONTEXT: ParseValueError = ParseValueError("Compression");
        const UNCOMPRESSED_SIZE_ERR: &'static str = "Failed to read uncompressed size";
        const ITEM_SIZE_ERR: &'static str = "Failed to read byte shuffling item size";
        fn parse_size(size: &str, err_msg: &'static str) -> Result<usize, Report<ParseValueError>> {
            parse_auto_radix::<usize>(size.trim())
                .into_report()
                .change_context(CONTEXT)
                .attach_printable(err_msg)
        }
        match s.split(":").collect::<Vec<_>>().as_slice() {
            &["zlib", uncompressed_size] => Ok(Self::Zlib(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?
            )),
            &["zlib+sh", uncompressed_size, item_size] => Ok(Self::ZlibByteShuffling(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?,
                parse_size(item_size, ITEM_SIZE_ERR)?
            )),
            &["lz4", uncompressed_size] => Ok(Self::Lz4(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?
            )),
            &["lz4+sh", uncompressed_size, item_size] => Ok(Self::Lz4ByteShuffling(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?,
                parse_size(item_size, ITEM_SIZE_ERR)?
            )),
            &["lz4hc", uncompressed_size] => Ok(Self::Lz4HC(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?
            )),
            &["lz4hc+sh", uncompressed_size, item_size] => Ok(Self::Lz4HCByteShuffling(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?,
                parse_size(item_size, ITEM_SIZE_ERR)?
            )),
            _bad => Err(report!(CONTEXT)).attach_printable(format!(
                "Unrecognized pattern: expected one of [zlib:len, zlib+sh:len:item-size, lz4:len, lz4+sh:len:item-size, lz4hc:len, lz4hc+sh:len:item-size], found {s}"
            ))
        }
    }
}

// tuples of (compressed size, uncompressed size)
#[derive(Clone, PartialEq)]
pub struct SubBlocks(pub Vec<(u64, u64)>);
impl fmt::Debug for SubBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl fmt::Display for SubBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.len() > 0 {
            let s = self.0.iter()
                .map(|(uncompressed_size, item_size)| format!("{uncompressed_size},{item_size}"))
                .reduce(|acc, next| format!("{acc}:{next}"))
                .unwrap(); // safe because the None only occurs when the iter is empty
            f.write_str(s.as_str())
        } else {
            f.write_str("")
        }
    }
}
impl FromStr for SubBlocks {
    type Err = Report<ParseValueError>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        const CONTEXT: ParseValueError = ParseValueError("Compression Sub-Blocks");
        let mut sub_blocks = vec![];
        for token in s.split(":") {
            if let Some((uncompressed_size, item_size)) = token.split_once(",") {
                sub_blocks.push((
                    parse_auto_radix::<u64>(uncompressed_size.trim())
                        .into_report()
                        .change_context(CONTEXT)?,

                    parse_auto_radix::<u64>(item_size.trim())
                        .into_report()
                        .change_context(CONTEXT)?,
                ));
            } else {
                return Err(report!(CONTEXT)).attach_printable(format!("Expected pattern x,i:y,j:..:z,k, found {s}"));
            }
        }
        Ok(Self(sub_blocks))
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub enum CompressionLevel {
    #[default]
    Auto,
    Value(NonZeroU8), // minimum: 1, maximum: 100
}
impl CompressionLevel {
    pub fn new(level: u8) -> Result<Self, Report<ParseValueError>> {
        match level {
            0 => Ok(Self::Auto),
            (1..=100) => Ok(Self::Value(NonZeroU8::new(level).unwrap())), // safe because the match arm range does not contain 0
            bad => Err(ParseValueError("CompressionLevel"))
                .into_report()
                .attach_printable(format!("Must be between 0 and 100, found {bad}"))
        }
    }
}