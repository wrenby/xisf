use std::{
    cell::RefCell,
    collections::HashMap,
    io::{self, Read, BufReader, Seek, Cursor},
    fmt,
    fs::File,
    num::{NonZeroU8, NonZeroUsize},
    path::PathBuf,
    rc::Rc,
    str::FromStr,
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
    pub location: Location,
    pub byte_order: ByteOrder,
    pub checksum: Option<Checksum>,
    pub compression: Option<Compression>,
}
impl DataBlock {
    // returns Ok(Some(_)) if a data block was successfully parsed
    // returns Ok(None) if there is no data block to parse
    // returns Err(_) if there was an error parsing the data block
    // passing &mut attrs isn't for the benefit of this function, but the caller function
    // (helps cut down on unnecessary "ignoring unrecognized attribute" warnings)
    pub(crate) fn parse_node(node: RoNode, xpath: &XpathContext, context: ParseNodeError, attrs: &mut HashMap<String, String>) -> Result<Option<Self>, Report<ParseNodeError>> {
        let _span_guard = tracing::debug_span!("DataBlock");
        if let Some(location) = Location::parse_node(node, xpath, context, attrs)? {
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

            let compression_attr = match attrs.remove("compression") {
                Some(compression) => Some(
                    compression.parse::<CompressionAttr>()
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
            let compression = {
                match (compression_attr, sub_blocks.0.len()) {
                    (Some(attr), 0) => {
                        Some(Compression {
                            algorithm: attr.algorithm(),
                            sub_blocks: SubBlocks(vec![
                                (usize::MAX, attr.uncompressed_size()) // TODO: usize::MAX here is safe, but it's a bit of a hack. marking just to verify it stays safe as I implement new features
                            ]),
                            byte_shuffling: attr.shuffle_item_size()
                        })
                    },
                    (Some(attr), _) => {
                        let uncompressed_size: usize = sub_blocks.0.iter().map(|(_, un)| un).sum();
                        if uncompressed_size != attr.uncompressed_size() {
                            return Err(report!(context))
                                .attach_printable("Compression sub-blocks must sum to the uncompressed size specified in the compression attribute")
                        }
                        Some(Compression {
                            algorithm: attr.algorithm(),
                            sub_blocks,
                            byte_shuffling: attr.shuffle_item_size()
                        })
                    },
                    (None, 0) => None,
                    (None, _) => {
                        tracing::warn!("Ignoring subblocks attribute because no compression was specified");
                        None
                    }
                }
            };

            Ok(Some(DataBlock {
                location,
                byte_order,
                checksum,
                compression,
            }))
        } else {
            Ok(None)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Location {
    /// Inline or embedded: data is encoded in a child text or <Data> node
    Plaintext {
        encoding: TextEncoding,
        text: String, // stripped of all whitespace
    },
    /// Data is elsewhere in the file (only supported for monolithic XISF files)
    Attachment {
        position: u64,
        size: u64,
    },
    /// Data is stored remotely (only supported for distributed XISF files)
    Url {
        url: Url,
        index_id: Option<u64>,
    },
    /// Data is store elsewhere on the filesystem (only supported for distributed XISF files)
    Path {
        path: PathBuf,
        index_id: Option<u64>,
    }
}
impl Location {
    /// returns Ok(Some(_)) if a data block location was successfully parsed
    /// returns Ok(None) if there is no location attribute
    /// returns Err(_) if there was an error parsing the data block location
    /// passing &mut attrs isn't for the benefit of this function, but the caller function
    /// (helps cut down on unnecessary "ignoring unrecognized attribute" warnings)
    pub(crate) fn parse_node(node: RoNode, _xpath: &XpathContext, context: ParseNodeError, attrs: &mut HashMap<String, String>) -> Result<Option<Self>, Report<ParseNodeError>> {
        let _span_guard = tracing::debug_span!("location");
        if let Some(attr) = attrs.remove("location") {
            match attr.split(":").collect::<Vec<_>>().as_slice() {
                &["inline", encoding] => {
                    let encoding = encoding.parse::<TextEncoding>()
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
                                let encoding = encoding.parse::<TextEncoding>()
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

    /// Literally just a byte stream, with no knowledge of compression, byte shuffling, or checksums
    pub(crate) fn raw_bytes(&self, xisf: &crate::XISF) -> Result<Box<dyn Read>, Report<ReadDataBlockError>> {
        let base64 = base64_simd::STANDARD;
        match self {
            Self::Plaintext { encoding, text } => {
                let buf = match encoding {
                    TextEncoding::Hex => hex_simd::decode_to_vec(text)
                        .into_report()
                        .change_context(ReadDataBlockError)?,
                    TextEncoding::Base64 => base64.decode_to_vec(text)
                        .into_report()
                        .change_context(ReadDataBlockError)?,
                };
                Ok(Box::new(Cursor::new(buf)))
            },
            Self::Attachment { position, size } => {
                let mut file = File::open(&xisf.filename)
                    .into_report()
                    .change_context(ReadDataBlockError)?;
                file.seek(io::SeekFrom::Start(*position))
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
            let uncompressed_sizes: Vec<_> = compression.sub_blocks.0.iter().map(|tup| tup.0).collect();
            match compression.algorithm {
                CompressionAlgorithm::Zlib => {
                    Ok(Box::new(
                        raw.sub_blocks(&uncompressed_sizes[..])
                            .map(|shared| {
                                Ok(Box::new(ZlibDecoder::new(shared)))
                            }).unwrap() // safe because the map function always succeeds, and that's the only point of failure
                    ))
                },
                CompressionAlgorithm::Lz4 | CompressionAlgorithm::Lz4HC => {
                    Ok(Box::new(
                        raw.sub_blocks(&uncompressed_sizes[..])
                            .map(|shared| {
                                Ok(Box::new(
                                    lz4::Decoder::new(shared)
                                        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?
                                ))
                            }).into_report()
                            .change_context(ReadDataBlockError)
                            .attach_printable("Failed to initialize Lz4 decoder")?
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

/// Allows a reader to be split at arbitrary byte offsets, kind of like a combination of `io::Take` and `io::Chain`
/// Used for streaming sub-block decompression
trait ReadSubBlocksExt {
    type Inner;
    fn sub_blocks(self, sizes: &[usize]) -> ReadSubBlocks<Self::Inner>;
}
impl<R: Read> ReadSubBlocksExt for R {
    type Inner = R;
    fn sub_blocks(self, sizes: &[usize]) -> ReadSubBlocks<Self::Inner> {
        ReadSubBlocks::new(self, sizes)
    }
}
struct ReadSubBlocks<T> {
    inner: SharedReader<T>,
    limits: Vec<usize>,
    chunk_index: usize,
}
impl<R: Read> ReadSubBlocks<R> {
    /// Panics if `sizes` is empty
    pub fn new(from: R, sizes: &[usize]) -> Self {
        Self {
            inner: SharedReader::new(from),
            limits: sizes.to_vec(),
            chunk_index: 0,
        }
    }

    /// Initializes the
    pub fn map<F>(self, f: F) -> io::Result<MapSubBlocks<F, R>>
        where F: Fn(SharedReader<R>) -> io::Result<Box<dyn Read>> {
        MapSubBlocks::new(self, f)
    }

    /// Number of bytes left to read before the current block ends
    pub fn current_limit(&self) -> usize {
        self.limits[self.chunk_index]
    }

    /// Necessary to call after a successful read to avoid corruption
    pub fn reduce_limit(&mut self, bytes_read: usize) {
        self.limits[self.chunk_index] -= bytes_read;
    }
}
struct SharedReader<T>(Rc<RefCell<T>>);
impl<T> Clone for SharedReader<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<T> SharedReader<T> {
    pub fn new(inner: T) -> Self {
        Self(Rc::new(RefCell::new(inner)))
    }
}
impl<R: Read> Read for SharedReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut lock = self.0.try_borrow_mut()
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        lock.read(buf)
    }
}
struct MapSubBlocks<F, R> {
    sub_blocks: ReadSubBlocks<R>,
    map_func: Box<F>,
    current: Box<dyn Read>,
}
impl<F, R> Read for MapSubBlocks<F, R>
    where F: Fn(SharedReader<R>) -> io::Result<Box<dyn Read>>, R: Read {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut total_bytes_read = 0;
        let size = self.sub_blocks.current_limit();
        let mut last_bytes_read = self.current.read(&mut buf[..size])?;
        while total_bytes_read != buf.len() {
            total_bytes_read += last_bytes_read;
            self.sub_blocks.reduce_limit(last_bytes_read);
            if last_bytes_read == 0 {
                if let Some(size) = self.next_block()? {
                    last_bytes_read = self.current.read(&mut buf[total_bytes_read..size])?;
                } else {
                    break;
                }
            }
        }
        Ok(total_bytes_read)
    }
}
impl<F, R> MapSubBlocks<F, R>
where F: Fn(SharedReader<R>) -> io::Result<Box<dyn Read>>, R: Read {
    pub fn new(sub_blocks: ReadSubBlocks<R>, f: F) -> io::Result<Self> {
        let current = f(sub_blocks.inner.clone())?;
        Ok(Self {
            sub_blocks,
            map_func: Box::new(f),
            current,
        })
    }
    /// Returns `None` if there are no blocks left
    pub fn next_block(&mut self) -> io::Result<Option<usize>> {
        self.sub_blocks.chunk_index += 1;
        if self.sub_blocks.chunk_index < self.sub_blocks.limits.len() {
            self.current = (*self.map_func)(self.sub_blocks.inner.clone())?;
            Ok(Some(self.sub_blocks.limits[self.sub_blocks.chunk_index]))
        } else {
            Ok(None)
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum TextEncoding {
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

#[derive(Clone, Debug, PartialEq)]
pub struct Compression {
    pub algorithm: CompressionAlgorithm,
    /// Will always have at least one element, even if no sub-blocks were specified
    /// in that case, sub-blocks will be one element, initialized with compressed-size taken from the data block,
    /// and uncompressed-size taken from the compression attribute
    /// * Not exposed in public API because compressed size is not always reliable
    pub(crate) sub_blocks: SubBlocks,
    /// The `NonZeroUsize` is the item size
    pub byte_shuffling: Option<NonZeroUsize>,
}
impl Compression {
    /// Calculated from the sum of sub-block uncompressed sizes. Not zero cost!
    pub fn uncompressed_size(&self) -> usize {
        self.sub_blocks.0.iter().map(|(_, un)| un).sum()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CompressionAlgorithm {
    Zlib,
    Lz4,
    Lz4HC,
}

/// Only used as an intermediate step in decoding, never exposed as part of the API
/// Enum fields follow the pattern uncompressed-size, byte-shuffling-item-size
#[derive(Clone, Debug, PartialEq)]
enum CompressionAttr {
    Zlib(usize),
    ZlibByteShuffling(usize, NonZeroUsize),
    Lz4(usize),
    Lz4ByteShuffling (usize, NonZeroUsize),
    Lz4HC(usize),
    Lz4HCByteShuffling(usize, NonZeroUsize),
}
impl CompressionAttr {
    pub fn algorithm(&self) -> CompressionAlgorithm {
        match self {
            Self::Zlib(_) | Self::ZlibByteShuffling(..) => CompressionAlgorithm::Zlib,
            Self::Lz4(_) | Self::Lz4ByteShuffling(..) => CompressionAlgorithm::Lz4,
            Self::Lz4HC(_) | Self::Lz4HCByteShuffling(..) => CompressionAlgorithm::Lz4HC,
        }
    }
    pub fn uncompressed_size(&self) -> usize {
        match self {
            &Self::Zlib(size) => size,
            &Self::ZlibByteShuffling(size, _) => size,
            &Self::Lz4(size) => size,
            &Self::Lz4ByteShuffling(size, _) => size,
            &Self::Lz4HC(size) => size,
            &Self::Lz4HCByteShuffling(size, _) => size,
        }
    }
    pub fn shuffle_item_size(&self) -> Option<NonZeroUsize> {
        match self {
            Self::Zlib(_) | Self::Lz4(_) | Self::Lz4HC(_) => None,
            &Self::ZlibByteShuffling(_, item_size) => Some(item_size),
            &Self::Lz4ByteShuffling(_, item_size) => Some(item_size),
            &Self::Lz4HCByteShuffling(_, item_size) => Some(item_size),
        }
    }
}
impl fmt::Display for CompressionAttr {
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
impl FromStr for CompressionAttr {
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
                NonZeroUsize::new(parse_size(item_size, ITEM_SIZE_ERR)?)
                    .ok_or(report!(CONTEXT))
                    .attach_printable("Byte shuffling item size cannot be zero")?
            )),
            &["lz4", uncompressed_size] => Ok(Self::Lz4(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?
            )),
            &["lz4+sh", uncompressed_size, item_size] => Ok(Self::Lz4ByteShuffling(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?,
                NonZeroUsize::new(parse_size(item_size, ITEM_SIZE_ERR)?)
                    .ok_or(report!(CONTEXT))
                    .attach_printable("Byte shuffling item size cannot be zero")?
            )),
            &["lz4hc", uncompressed_size] => Ok(Self::Lz4HC(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?
            )),
            &["lz4hc+sh", uncompressed_size, item_size] => Ok(Self::Lz4HCByteShuffling(
                parse_size(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?,
                NonZeroUsize::new(parse_size(item_size, ITEM_SIZE_ERR)?)
                    .ok_or(report!(CONTEXT))
                    .attach_printable("Byte shuffling item size cannot be zero")?
            )),
            _bad => Err(report!(CONTEXT)).attach_printable(format!(
                "Unrecognized pattern: expected one of [zlib:len, zlib+sh:len:item-size, lz4:len, lz4+sh:len:item-size, lz4hc:len, lz4hc+sh:len:item-size], found {s}"
            ))
        }
    }
}

/// Tuples of (compressed size, uncompressed size)
#[derive(Clone, PartialEq)]
pub struct SubBlocks(pub Vec<(usize, usize)>); // TODO: NonZeroUsize, also consider if putting in the work to change this to u64 actually gets me anything
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
                    parse_auto_radix::<usize>(uncompressed_size.trim())
                        .into_report()
                        .change_context(CONTEXT)?,

                    parse_auto_radix::<usize>(item_size.trim())
                        .into_report()
                        .change_context(CONTEXT)?,
                ));
            } else {
                return Err(report!(CONTEXT)).attach_printable(format!("Expected pattern x,i:y,j:...:z,k, found {s}"));
            }
        }
        if sub_blocks.len() == 0 {
            return Err(report!(CONTEXT)).attach_printable("Requires at least one compressed-size,uncompressed-size pair");
        } else {
            Ok(Self(sub_blocks))
        }
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