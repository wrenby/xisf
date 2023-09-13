//! Anything related to [`DataBlock`], XISF's representation of bulk binary data

use std::{
    cell::RefMut,
    collections::HashMap,
    io::{self, Read, BufReader, Seek, Cursor, Write, Take},
    fmt,
    fs::File,
    num::NonZeroU64,
    path::PathBuf,
    str::FromStr,
};

use digest::Digest;
use error_stack::{Report, Result, ResultExt, report};
use flate2::read::ZlibDecoder;
use libxml::{readonly::RoNode, tree::NodeType};
use ndarray::Array2;
use parse_int::parse as parse_auto_radix;
use remotefs::{RemoteError, RemoteErrorType};
use sha1::Sha1;
use sha2::{Sha256, Sha512};
use sha3::{Sha3_256, Sha3_512};
use strum::{EnumString, Display, EnumVariantNames};
use url::Url;
use crate::error::{
    ParseValueError,
    ParseNodeError,
    ParseNodeErrorKind::{self, *},
    ReadDataBlockError,
};

mod context;
pub use context::*;

mod sub_blocks;
use sub_blocks::*;

/// The XISF file format's representation of any kind of binary data
///
/// Most commonly used for [images](crate::image::Image), this type is essentially
/// a reference to a file or part of a file where the raw data can be read from.
#[derive(Debug, Clone, PartialEq)]
pub struct DataBlock {
    /// Where this data block can be found
    pub location: Location,
    /// The byte order/endianness of this data block
    pub byte_order: ByteOrder,
    /// A checksum that the data must match, if one exists
    pub checksum: Option<Checksum>,
    /// The type of compression that was used, if any
    pub compression: Option<Compression>,
}
impl DataBlock {
    // returns Ok(Some(_)) if a data block was successfully parsed
    // returns Ok(None) if there was no data block to parse
    // returns Err(_) if there was an error parsing the data block
    // passing &mut attrs isn't for the benefit of this function, but the caller function
    // (helps cut down on unnecessary "ignoring unrecognized attribute" warnings)
    pub(crate) fn parse_node(node: RoNode, tag: &'static str, attrs: &mut HashMap<String, String>) -> Result<Option<Self>, ParseNodeError> {
        let context = |kind| -> ParseNodeError {
            ParseNodeError::new(tag, kind)
        };
        let report = |kind: ParseNodeErrorKind| -> Report<ParseNodeError> {
            report!(ParseNodeError::new(tag, kind))
        };

        if let Some(location) = Location::parse_node(node, tag, attrs)? {
            let byte_order = match attrs.remove("byteOrder") {
                Some(byte_order) => {
                    byte_order.parse::<ByteOrder>()
                        .change_context(context(InvalidAttr))
                        .attach_printable_lazy(|| format!("Invalid byteOrder attribute: expected one of [big, little], found {byte_order}"))?
                },
                None => Default::default(),
            };

            let checksum = match attrs.remove("checksum") {
                Some(checksum) => Some(
                    checksum.parse::<Checksum>()
                        .change_context(context(InvalidAttr))
                        .attach_printable("Invalid checksum attribute")?
                ),
                None => None,
            };

            let compression_attr = match attrs.remove("compression") {
                Some(compression) => Some(
                    compression.parse::<CompressionAttr>()
                        .change_context(context(InvalidAttr))
                        .attach_printable("Invalid compression attribute")?
                ),
                None => None,
            };

            let sub_blocks = match attrs.remove("subblocks") {
                Some(compression) => {
                    compression.parse::<SubBlocks>()
                        .change_context(context(InvalidAttr))
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
                                (u64::MAX, attr.uncompressed_size()) // TODO: u64::MAX here is safe, but it's a bit of a hack. marking just to verify it stays safe as I implement new features
                            ]),
                            byte_shuffling: attr.shuffle_item_size()
                        })
                    },
                    (Some(attr), _) => {
                        let uncompressed_size: u64 = sub_blocks.0.iter().map(|(_, un)| un).sum();
                        if uncompressed_size != attr.uncompressed_size() {
                            return Err(report(InvalidAttr))
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

    pub(crate) fn verify_checksum(&self, ctx: &Context) -> Result<(), ReadDataBlockError> {
        fn verify_checksum_impl<D: Digest + Write>(expected: &[u8], reader: &mut impl Read) -> Result<(), ReadDataBlockError> {
            let mut hasher = D::new();
            std::io::copy(reader, &mut hasher)
                .change_context(ReadDataBlockError::IoError)
                .attach_printable("Failed to calculate data block hash")?;
            let actual = hasher.finalize();
            if actual.as_slice() == expected {
                Ok(())
            } else {
                let actual = hex_simd::encode_to_string(actual.as_slice(), hex_simd::AsciiCase::Lower);
                let expected = hex_simd::encode_to_string(expected, hex_simd::AsciiCase::Lower);
                Err(report!(ReadDataBlockError::DifferentChecksum))
                    .attach_printable(format!("Data block failed checksum verification: expected {expected}, found {actual}"))
            }
        }

        if let Some(checksum) = &self.checksum {
            let mut reader = self.location.raw_bytes(&ctx)?;
            match checksum {
                Checksum::Sha1(digest) => verify_checksum_impl::<Sha1>(digest, &mut reader),
                Checksum::Sha256(digest) => verify_checksum_impl::<Sha256>(digest, &mut reader),
                Checksum::Sha512(digest) => verify_checksum_impl::<Sha512>(digest, &mut reader),
                Checksum::Sha3_256(digest) => verify_checksum_impl::<Sha3_256>(digest, &mut reader),
                Checksum::Sha3_512(digest) => verify_checksum_impl::<Sha3_512>(digest, &mut reader),
            }
        } else {
            Ok(())
        }
    }

    /// Will duplicate in-memory if byte-shuffling is enabled
    pub(crate) fn decompressed_bytes<'a>(&self, ctx: &'a Context) -> Result<Box<dyn Read + 'a>, ReadDataBlockError> {
        self.location.decompressed_bytes(ctx, &self.compression)
    }
}

/// Where to find this data block
#[derive(Clone, Debug, PartialEq)]
pub enum Location {
    /// Inline or embedded: data is encoded in a child text or &lt;Data&gt; node
    Text {
        ///
        encoding: TextEncoding,
        /// The text itself, stripped of all whitespace
        text: String,
    },
    /// Data is elsewhere in the file (only supported for monolithic XISF files)
    Attachment {
        /// The byte offset of this data block, relative to the start of the file
        position: u64,
        /// The length in bytes of this data block
        size: u64,
    },
    /// Data is stored remotely (only supported for distributed XISF files)
    Url {
        /// The URL where a file storing this data block can be found
        url: Url,
        /// If `Some`, indicates that the file at the given URL is an [XISB file](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__XISF_Structure_:_XISF_Data_Blocks_File__),
        /// where the contained `u64` is the unique ID of the block index element pointing to the desired section of the XISB file.
        /// If `None`, indicates that the entire file is one big data block.
        index_id: Option<u64>,
    },
    /// Data is store elsewhere on the filesystem (only supported for distributed XISF files)
    Path {
        /// The path where a file storing this data block can be found
        path: PathBuf,
        /// If `Some`, indicates that the file at the given path is an [XISB file](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__XISF_Structure_:_XISF_Data_Blocks_File__),
        /// where the contained `u64` is the unique ID of the block index element pointing to the desired section of the XISB file.
        /// If `None`, indicates that the entire file is one big data block.
        index_id: Option<u64>,
    }
}
impl Location {
    /// returns Ok(Some(_)) if a data block location was successfully parsed
    /// returns Ok(None) if there is no location attribute
    /// returns Err(_) if there was an error parsing the data block location
    /// passing &mut attrs isn't for the benefit of this function, but the caller function
    /// (helps cut down on unnecessary "ignoring unrecognized attribute" warnings)
    pub(crate) fn parse_node(node: RoNode, tag: &'static str, attrs: &mut HashMap<String, String>) -> Result<Option<Self>, ParseNodeError> {
        let context = |kind| -> ParseNodeError {
            ParseNodeError::new(tag, kind)
        };
        let report = |kind: ParseNodeErrorKind| -> Report<ParseNodeError> {
            report!(ParseNodeError::new(tag, kind))
        };

        if let Some(attr) = attrs.remove("location") {
            match attr.split(":").collect::<Vec<_>>().as_slice() {
                &["inline", encoding] => {
                    let encoding = encoding.parse::<TextEncoding>()
                        .change_context(context(InvalidAttr))
                        .attach_printable("Invalid location attribute: failed to parse inline encoding")?;

                    match node.get_child_nodes().as_slice() {
                        [] => Err(report(MissingChild)).attach_printable("Missing child text node: required for inline data blocks"),
                        [text] if text.get_type() == Some(NodeType::TextNode) => {
                            let mut text = text.get_content();
                            text.retain(|c| !c.is_whitespace());
                            Ok(Some(
                                Self::Text {
                                    encoding,
                                    text,
                                }
                            ))
                        },
                        _other => Err(report(InvalidChild)).attach_printable("XISF Elements with inline data blocks are not permitted to have non-text child nodes"),
                    }
                },
                &["embedded"] =>  {
                    match node.get_child_nodes()
                        .into_iter()
                        .filter(|n| n.get_name() == "Data")
                        .collect::<Vec<_>>()
                        .as_slice()
                    {
                        [] => Err(report(MissingChild)).attach_printable("Missing embedded <Data> node: required for embedded data block location"),
                        [one] => {
                            if let Some(encoding) = one.get_attribute("encoding") {
                                let encoding = encoding.parse::<TextEncoding>()
                                    .change_context(context(InvalidAttr))
                                    .attach_printable("Invalid encoding attribute in embedded <Data> node")?;

                                match one.get_child_nodes().as_slice() {
                                    [] => Err(report(MissingChild)).attach_printable("Embedded <Data> node missing child text node"),
                                    [text] if text.get_type() == Some(NodeType::TextNode) => {
                                        let mut text = text.get_content();
                                        text.retain(|c| !c.is_whitespace());
                                        Ok(Some(
                                            Self::Text {
                                                encoding,
                                                text,
                                            }
                                        ))
                                    },
                                    _other => Err(report(InvalidChild)).attach_printable("Embedded <Data> nodes are not permitted to have non-text child nodes"),
                                }
                            } else {
                                Err(report(MissingAttr)).attach_printable("Embedded <Data> node missing encoding attribute")
                            }
                        },
                        _many => Err(report(InvalidChild)).attach_printable("Found more than one embedded <Data> node"),
                    }
                },
                &["attachment", position, size] => {
                    Ok(Some(Self::Attachment {
                        position: parse_auto_radix::<u64>(position.trim())
                            .change_context(context(InvalidAttr))
                            .attach_printable("Invalid location attribute: failed to parse position of attached data block")?,
                        size: parse_auto_radix::<u64>(size.trim())
                            .change_context(context(InvalidAttr))
                            .attach_printable("Invalid location attribute: failed to parse size of attached data block")?,
                    }))
                },
                &[url] if url.starts_with("url(") && url.ends_with(")") => {
                    // parentheses in url must be encoded with XML character references #&40; and &#41;,
                    // but libxml handles that for us transparently
                    Ok(Some(Self::Url {
                        // the slice indexing trims "url(" from the front and ")" from the end
                        url: Url::parse(&url[4..url.len()-1])
                            .change_context(context(InvalidAttr))
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
                            .change_context(context(InvalidAttr))
                            .attach_printable("Invalid location attribute: failed to parse URL of external data block")?,
                        index_id: Some(parse_auto_radix::<u64>(index_id.trim())
                            .change_context(context(InvalidAttr))
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
                            .change_context(context(InvalidAttr))
                            .attach_printable("Invalid location attribute: failed to parse index-id of external data block")?),
                    }))
                },
                _bad => Err(report(InvalidAttr)).attach_printable("Invalid location attribute: unrecognized pattern")
                    .attach_printable(format!("Expected one of [inline:encoding, embedded, attachment:position:size, url(...), url(...):index-id, path(...), path(...):index-id], found {attr}"))
            }
        } else {
            Ok(None)
        }
    }

    /// Literally just a byte stream, with no knowledge of compression, byte shuffling, or checksums
    pub(crate) fn raw_bytes<'a>(&self, ctx: &'a Context) -> Result<Box<dyn Read + 'a>, ReadDataBlockError> {
        match self {
            Self::Text { encoding, text } => {
                let buf = match encoding {
                    TextEncoding::Hex => hex_simd::decode_to_vec(text)
                        .change_context(ReadDataBlockError::BadTextEncoding)
                        .attach_printable("Bad hex encoding")?,
                    TextEncoding::Base64 => base64_simd::STANDARD.decode_to_vec(text)
                        .change_context(ReadDataBlockError::BadTextEncoding)
                        .attach_printable("Bad Base64 encoding")?,
                };
                Ok(Box::new(Cursor::new(buf)))
            },
            Self::Attachment { position, size } => {
                if let Source::Monolithic(cell) = &ctx.source {
                    let mut reader = cell.try_borrow_mut()
                        .change_context(ReadDataBlockError::FileInUse)?;
                    reader.seek(io::SeekFrom::Start(*position))
                        .change_context(ReadDataBlockError::IoError)?;
                    Ok(Box::new(reader.take_ref_mut(*size)))
                }  else {
                    Err(report!(ReadDataBlockError::UnsupportedLocation))
                        .attach_printable("Data blocks with location=\"attachment\" are only supported for monolithic files")
                }
            },
            Self::Url { url, index_id: None } => {
                if let Source::Distributed(_) = &ctx.source {
                    if let Some(host) = url.host() {
                        ctx.ensure_trusted(host)?;
                    }
                    match url.scheme() {
                        #[cfg(feature = "remote-http")]
                        "http" | "https" => {
                            let resp = ureq::get(url.as_str())
                                .call()
                                .change_context(ReadDataBlockError::IoError)?;
                            Ok(resp.into_reader())
                        },
                        #[cfg(feature = "remote-ftp")]
                        "ftp" => {
                            use remotefs::RemoteFs;
                            const DEFAULT_FTP_PORT: u16 = 21;
                            let host = url.host().ok_or(report!(ReadDataBlockError::MissingHost))?;
                            let mut ftp = remotefs_ftp::FtpFs::new(
                                host.to_string(),
                                url.port().unwrap_or(DEFAULT_FTP_PORT)
                            ).username(url.username())
                            .password(url.password().unwrap_or(""));
                            match ftp.connect() {
                                Ok(_) => {},
                                Err(RemoteError { kind: RemoteErrorType::AuthenticationFailed, ..}) => {
                                    return Err(report!(ReadDataBlockError::Unauthorized(url.clone())));
                                },
                                Err(_) => return Err(report!(ReadDataBlockError::IoError)).attach_printable("Failed to connect to FTP server"),
                            }
                            let file = ftp.open(url.path().as_ref())
                                .change_context(ReadDataBlockError::IoError)
                                .attach_printable("Failed to open file over FTP")?;
                            Ok(Box::new(file))
                        },
                        bad => Err(report!(ReadDataBlockError::UnsupportedScheme(bad.to_string())))
                            .attach_printable(format!("Unsupported scheme: {bad}"))
                    }
                }  else {
                    Err(report!(ReadDataBlockError::UnsupportedLocation))
                        .attach_printable("Data blocks with location=\"url(...)\" are only supported for distributed files")
                }
            },
            #[allow(unused_variables)]
            Self::Url { url, index_id: Some(idx) } => {
                todo!()
            },
            Self::Path { path, index_id: None } => {
                if let Source::Distributed(directory) = &ctx.source {
                    if path.starts_with("@header_dir/") {
                        let mut path_buf = directory.clone();
                        // this unwrap is safe because we just checked that it starts with @header_dir/
                        path_buf.push(path.strip_prefix("@header_dir/").unwrap());
                        let file = File::open(path_buf)
                            .change_context(ReadDataBlockError::IoError)?;
                        Ok(Box::new(BufReader::new(file)))
                    } else {
                        let file = File::open(path)
                            .change_context(ReadDataBlockError::IoError)?;
                        Ok(Box::new(BufReader::new(file)))
                    }
                } else {
                    Err(report!(ReadDataBlockError::UnsupportedLocation))
                        .attach_printable("Data blocks with location=\"path(...)\" are only supported for distributed files")
                }
            },
            #[allow(unused_variables)]
            Self::Path { path, index_id: Some(idx) } => {
                todo!()
            },
        }
    }

    /// Will duplicate in-memory if byte-shuffling is enabled
    pub(crate) fn decompressed_bytes<'a>(&self, ctx: &'a Context, compression: &Option<Compression>) -> Result<Box<dyn Read + 'a>, ReadDataBlockError> {
        let raw = self.raw_bytes(ctx)?;
        if let Some(compression) = compression {
            let uncompressed_sizes: Vec<_> = compression.sub_blocks.0.iter().map(|tup| tup.0).collect();
            match compression.algorithm {
                CompressionAlgorithm::Zlib => {
                    let zlib = raw.multi_take(uncompressed_sizes)
                        .map(|shared| {
                            Ok(ZlibDecoder::new(shared))
                        }).multi_chain()
                        .unwrap();

                    Self::unshuffle(zlib, compression)
                },
                CompressionAlgorithm::Lz4 | CompressionAlgorithm::Lz4HC => {
                    let lz4 = raw.multi_take(uncompressed_sizes)
                        .map(|shared| {
                            Ok(
                                lz4::Decoder::new(shared)
                                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?
                            )
                        }).multi_chain()
                        .change_context(ReadDataBlockError::IoError)
                        .attach_printable("Failed to initialize Lz4 decoder")?;

                    Self::unshuffle(lz4, compression)
                },
                CompressionAlgorithm::Zstd => {
                    let zstd = raw.multi_take(uncompressed_sizes)
                        .map(|shared| {
                            Ok(
                                zstd::Decoder::new(shared)
                                    .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?
                            )
                        }).multi_chain()
                        .change_context(ReadDataBlockError::IoError)
                        .attach_printable("Failed to initialize Zstd decoder")?;

                    Self::unshuffle(zstd, compression)
                },
            }
        } else {
            Ok(raw)
        }
    }

    // TODO: does it make any sense to unshuffle in-place reading one byte at a time?
    // would have to make a wrapper with a custom Read impl to even test it
    fn unshuffle<'a>(mut reader: impl Read + 'a, compression: &Compression) -> Result<Box<dyn Read + 'a>, ReadDataBlockError> {
        const ONE: NonZeroU64 = unsafe { NonZeroU64::new_unchecked(1) };
        match compression.byte_shuffling {
            // byte shuffling is a nop for 1 or 0 size items
            // not sure why any implementation would encode it like this, but best to save the clone I guess
            Some(item_size) if item_size > ONE => {
                let item_size: u64 = item_size.into();
                let n = compression.uncompressed_size() / item_size;
                if n * item_size != compression.uncompressed_size() {
                    return Err(report!(ReadDataBlockError::BadByteShuffleItemSize))
                }
                // to unshuffle, call this same code block [n, item_size] instead of [item_size, n]
                let mut buf = Array2::<u8>::zeros([n as usize, item_size as usize]);
                reader.read_exact(buf.as_slice_memory_order_mut().unwrap())
                    .change_context(ReadDataBlockError::IoError)
                    .attach_printable("Failed to read bytes into temporary buffer for unshuffling")?;
                buf.swap_axes(0, 1);
                Ok(Box::new(Cursor::new(buf.as_standard_layout().to_owned().into_raw_vec())))
            },
            _ => Ok(Box::new(reader))
        }
    }
}

/// A wrapper which gives [`RefMut`] a [`Read`] implementation when its inner type has one,
/// which allows [`Read::take()`] to be called on it without dereferencing to an unwanted local lifetime
pub(super) struct RefMutReader<'a, R>(RefMut<'a, R>);
impl<'a, R> Read for RefMutReader<'a, R> where R: Read {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
}

pub(super) trait ReadTakeRefExt<'a, R> {
    fn take_ref_mut(self, limit: u64) -> Take<RefMutReader<'a, R>>;
}
impl<'a, R> ReadTakeRefExt<'a, R> for RefMut<'a, R> where R: Read {
    fn take_ref_mut(self, limit: u64) -> Take<RefMutReader<'a, R>> {
        RefMutReader(self).take(limit)
    }
}

/// Describes the encoding of an [inline or embedded](Location::Text) data block
#[derive(Clone, Copy, Debug, Default, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum TextEncoding {
    /// [Base 64 encoding](https://datatracker.ietf.org/doc/html/rfc4648#section-4)
    #[default]
    #[strum(serialize = "base64")]
    Base64,
    /// [Hexadecimal (base 16) encoding](https://datatracker.ietf.org/doc/html/rfc4648#section-8), must be serialized with a-f in lowercase
    #[strum(serialize = "hex")]
    Hex,
}

/// The byte order (AKA endianness) of this data block
#[derive(Clone, Copy, Debug, Default, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum ByteOrder {
    /// Big endian (most significant bytes are stored first)
    #[strum(serialize = "big")]
    Big,
    /// Little endian (least significant bytes are stored first)
    #[default]
    #[strum(serialize = "little")]
    Little,
}

/// A cryptographic hash function used to compute a [data block](DataBlock)'s [checksum](Checksum)
#[derive(Clone, Copy, Debug, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum ChecksumAlgorithm {
    /// The SHA-1 cryptographic hash function
    #[strum(serialize = "sha-1", serialize = "sha1")]
    Sha1,
    /// The SHA-256 cryptographic hash function
    #[strum(serialize = "sha-256", serialize = "sha256")]
    Sha256,
    /// The SHA-512 cryptographic hash function
    #[strum(serialize = "sha-512", serialize = "sha512")]
    Sha512,
    /// The SHA3-256 cryptographic hash function
    #[strum(serialize = "sha3-256")]
    Sha3_256,
    /// The SHA3-512 cryptographic hash function
    #[strum(serialize = "sha3-512")]
    Sha3_512,
}

/// A checksum digest for a [data block](DataBlock) with a given algorithm
#[derive(Clone, Debug, PartialEq)]
pub enum Checksum {
    /// A 20-byte digest for the SHA-1 cryptographic hash function
    Sha1([u8; 20]),
    /// A 32-byte digest for the SHA-256 cryptographic hash function
    Sha256([u8; 32]),
    /// A 64-byte digest for the SHA-512 cryptographic hash function
    Sha512([u8; 64]),
    /// A 32-byte digest for the SHA3-256 cryptographic hash function
    Sha3_256([u8; 32]),
    /// A 64-byte digest for the SHA3-512 cryptographic hash function
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
    fn from_str(s: &str) -> Result<Self, ParseValueError> {
        const CONTEXT: ParseValueError = ParseValueError("Checksum");

        fn from_hex(digest: &str, out: &mut [u8]) -> Result<(), ParseValueError> {
            use hex_simd::AsOut;
            // the comment on this function says it panics if the dest buffer is not large enough,
            // but this is not true -- it returns an Err
            hex_simd::decode(digest.as_bytes(), out[..].as_out())
                .map(|_| ())
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
    /// Returns a slice to the digest
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

/// All configuration options pertaining to sub-block compression
#[derive(Clone, Debug, PartialEq)]
pub struct Compression {
    /// The algorithm used to compress this block
    pub algorithm: CompressionAlgorithm,
    /// Will always have at least one element, even if no sub-blocks were specified
    /// in that case, sub-blocks will be one element, initialized with compressed-size taken from the data block,
    /// and uncompressed-size taken from the compression attribute
    ///
    /// <div class="warning">
    ///
    /// In the event that the sub-blocks attribute was not specified, and the data block is not [`attached`](Location::Attachment),
    /// there is no way to know the compressed size of the block, and that size will instead be [`u64::MAX`].
    /// The compressed size should not be relied on for
    ///
    /// </div>
    pub(crate) sub_blocks: SubBlocks,
    /// If `Some`, the `NonZeroUsize` is the item size.
    /// If `None`, this block is not using byte-shuffling.
    pub byte_shuffling: Option<NonZeroU64>,
}
impl Compression {
    /// Calculated from the sum of sub-block uncompressed sizes. Not zero cost!
    pub fn uncompressed_size(&self) -> u64 {
        self.sub_blocks.0.iter().map(|(_, un)| un).sum()
    }
}

/// An algorithm used to compress or decompress a [data block](DataBlock)
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CompressionAlgorithm {
    /// [Zlib](https://datatracker.ietf.org/doc/html/rfc1950)
    Zlib,
    /// [LZ4](https://lz4.org/)
    Lz4,
    /// Lz4 High-Compression, a variant of [LZ4](Self::Lz4) which sacrifices speed for an improved compression ratio
    Lz4HC,
    /// [Zstandard, AKA zstd](https://datatracker.ietf.org/doc/html/rfc8478)
    Zstd,
}

/// Only used as an intermediate step in decoding, never exposed as part of the API
/// Enum fields follow the pattern uncompressed-size, byte-shuffling-item-size
#[derive(Clone, Debug, PartialEq)]
enum CompressionAttr {
    Zlib(u64),
    ZlibByteShuffling(u64, NonZeroU64),
    Lz4(u64),
    Lz4ByteShuffling(u64, NonZeroU64),
    Lz4HC(u64),
    Lz4HCByteShuffling(u64, NonZeroU64),
    Zstd(u64),
    ZstdByteShuffling(u64, NonZeroU64),
}
impl CompressionAttr {
    pub fn algorithm(&self) -> CompressionAlgorithm {
        match self {
            Self::Zlib(_) | Self::ZlibByteShuffling(..) => CompressionAlgorithm::Zlib,
            Self::Lz4(_) | Self::Lz4ByteShuffling(..) => CompressionAlgorithm::Lz4,
            Self::Lz4HC(_) | Self::Lz4HCByteShuffling(..) => CompressionAlgorithm::Lz4HC,
            Self::Zstd(_) | Self::ZstdByteShuffling(..) => CompressionAlgorithm::Zstd,
        }
    }
    pub fn uncompressed_size(&self) -> u64 {
        match self {
            &Self::Zlib(size) => size,
            &Self::ZlibByteShuffling(size, _) => size,
            &Self::Lz4(size) => size,
            &Self::Lz4ByteShuffling(size, _) => size,
            &Self::Lz4HC(size) => size,
            &Self::Lz4HCByteShuffling(size, _) => size,
            &Self::Zstd(size) => size,
            &Self::ZstdByteShuffling(size, _) => size,
        }
    }
    pub fn shuffle_item_size(&self) -> Option<NonZeroU64> {
        match self {
            Self::Zlib(_) | Self::Lz4(_) | Self::Lz4HC(_) | Self::Zstd(_) => None,
            &Self::ZlibByteShuffling(_, item_size) => Some(item_size),
            &Self::Lz4ByteShuffling(_, item_size) => Some(item_size),
            &Self::Lz4HCByteShuffling(_, item_size) => Some(item_size),
            &Self::ZstdByteShuffling(_, item_size) => Some(item_size),
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
            Self::Zstd(uncompressed_size) =>
                f.write_fmt(format_args!("zstd:{uncompressed_size}")),
            Self::ZstdByteShuffling(uncompressed_size, item_size) =>
                f.write_fmt(format_args!("zstd+sh:{uncompressed_size}:{item_size}")),
        }
    }
}
impl FromStr for CompressionAttr {
    type Err = Report<ParseValueError>;
    fn from_str(s: &str) -> Result<Self, ParseValueError> {
        const CONTEXT: ParseValueError = ParseValueError("Compression");
        const UNCOMPRESSED_SIZE_ERR: &'static str = "Failed to read uncompressed size";
        const ITEM_SIZE_ERR: &'static str = "Failed to read byte shuffling item size";
        fn parse_u64(size: &str, err_msg: &'static str) -> Result<u64, ParseValueError> {
            parse_auto_radix::<u64>(size.trim())
                .change_context(CONTEXT)
                .attach_printable(err_msg)
        }
        match s.split(":").collect::<Vec<_>>().as_slice() {
            &["zlib", uncompressed_size] => Ok(Self::Zlib(
                parse_u64(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?
            )),
            &["zlib+sh", uncompressed_size, item_size] => Ok(Self::ZlibByteShuffling(
                parse_u64(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?,
                NonZeroU64::new(parse_u64(item_size, ITEM_SIZE_ERR)?)
                    .ok_or(report!(CONTEXT))
                    .attach_printable("Byte shuffling item size cannot be zero")?
            )),
            &["lz4", uncompressed_size] => Ok(Self::Lz4(
                parse_u64(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?
            )),
            &["lz4+sh", uncompressed_size, item_size] => Ok(Self::Lz4ByteShuffling(
                parse_u64(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?,
                NonZeroU64::new(parse_u64(item_size, ITEM_SIZE_ERR)?)
                    .ok_or(report!(CONTEXT))
                    .attach_printable("Byte shuffling item size cannot be zero")?
            )),
            &["lz4hc", uncompressed_size] => Ok(Self::Lz4HC(
                parse_u64(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?
            )),
            &["lz4hc+sh", uncompressed_size, item_size] => Ok(Self::Lz4HCByteShuffling(
                parse_u64(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?,
                NonZeroU64::new(parse_u64(item_size, ITEM_SIZE_ERR)?)
                    .ok_or(report!(CONTEXT))
                    .attach_printable("Byte shuffling item size cannot be zero")?
            )),
            &["zstd", uncompressed_size] => Ok(Self::Zstd(
                parse_u64(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?
            )),
            &["zstd+sh", uncompressed_size, item_size] => Ok(Self::ZstdByteShuffling(
                parse_u64(uncompressed_size, UNCOMPRESSED_SIZE_ERR)?,
                NonZeroU64::new(parse_u64(item_size, ITEM_SIZE_ERR)?)
                    .ok_or(report!(CONTEXT))
                    .attach_printable("Byte shuffling item size cannot be zero")?
            )),
            _bad => Err(report!(CONTEXT)).attach_printable(format!(
                "Unrecognized pattern: expected one of [zlib:len, zlib+sh:len:item-size, lz4:len, lz4+sh:len:item-size, lz4hc:len, lz4hc+sh:len:item-size, zstd:len, zstd+sh:len:item-size], found {s}"
            ))
        }
    }
}

/// Tuples of (compressed size, uncompressed size)
#[derive(Clone, PartialEq)]
pub(crate) struct SubBlocks(pub(crate) Vec<(u64, u64)>);
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
    fn from_str(s: &str) -> Result<Self, ParseValueError> {
        const CONTEXT: ParseValueError = ParseValueError("Compression Sub-Blocks");
        let mut sub_blocks = vec![];
        for token in s.split(":") {
            if let Some((uncompressed_size, item_size)) = token.split_once(",") {
                sub_blocks.push((
                    parse_auto_radix::<u64>(uncompressed_size.trim())
                        .change_context(CONTEXT)?,

                    parse_auto_radix::<u64>(item_size.trim())
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

/// An codec-independent value between 1 and 100 indicating the trade-off between speed and compression ratio.
///
/// A low value sacrifices compression ratio for speed, a high value sacrifices speed for compression ratio.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CompressionLevel(u8);
impl CompressionLevel {
    /// Selects a
    // Zlib => 6
    // Lz4 => 64
    // Lz4HC => 9
    // Zstd => 3 (zstd::DEFAULT_COMPRESSION_LEVEL)
    pub const AUTO: Self = Self(0);
}
impl CompressionLevel {
    /// Create a new `CompressionLevel`
    ///
    /// Fails if `level` is outside the range `1..=100`
    pub fn new(level: u8) -> Result<Self, ParseValueError> {
        match level {
            val @ 1..=100 => Ok(Self(val)),
            bad => Err(ParseValueError("CompressionLevel"))
                .attach_printable(format!("Must be between 1 and 100, found {bad}"))
        }
    }
}
impl Default for CompressionLevel {
    fn default() -> Self {
        Self::AUTO
    }
}

#[cfg(test)]
mod tests {
    use ndarray::Array3;

    use super::*;

    const GRADIENT_SIZE: u64 = 250 * 200 * 3;
    fn check_gradient(arr: &Array3<u8>) {
        for ((x, y, z), v) in arr.indexed_iter() {
            match z {
                0 => assert_eq!(*v, y as u8),
                1 => assert_eq!(*v, x as u8),
                2 => assert_eq!(*v, 255 - (x as u8).min(y as u8)),
                _ => unreachable!(),
            };
        }
    }

    #[test]
    fn plaintext() {
        use hex_simd::AsciiCase;
        let data: Vec<_> = (0u8..=255u8).collect();
        let text = hex_simd::encode_to_string(&data, AsciiCase::Lower);
        let hex = DataBlock {
            location: Location::Text { encoding: TextEncoding::Hex, text },
            byte_order: ByteOrder::Little, // doesn't matter, since we're using raw_bytes
            checksum: None,
            compression: None,
        };
        let ctx = Context::distributed("tests/files/");
        let mut reader = hex.location.raw_bytes(&ctx).unwrap();
        let mut array = [0u8; 256];
        reader.read_exact(&mut array).unwrap();
        assert_eq!(array, &data[..]);

        let text = base64_simd::STANDARD.encode_to_string(&data);
        let base64 = DataBlock {
            location: Location::Text { encoding: TextEncoding::Base64, text },
            ..hex
        };
        let mut reader = base64.location.raw_bytes(&ctx).unwrap();
        let mut array = [0u8; 256];
        reader.read_exact(&mut array).unwrap();
        assert_eq!(array, &data[..]);
    }

    #[test]
    fn attachment() {
        let attachment = DataBlock {
            location: Location::Attachment { position: 0, size: GRADIENT_SIZE },
            byte_order: ByteOrder::Little, // doesn't matter, since we're using raw_bytes
            checksum: None,
            compression: None,
        };

        // not supported for distributed files
        let ctx = Context::distributed("tests/files/");
        let err = attachment.location.raw_bytes(&ctx).err().unwrap();
        assert_eq!(err.current_context(), &ReadDataBlockError::UnsupportedLocation);

        let file = File::open("tests/files/gradient.bin").unwrap();
        let buf_read = BufReader::new(file);
        let ctx = Context::monolithic(buf_read);
        let mut reader = attachment.location.raw_bytes(&ctx).unwrap();
        let mut array: Array3<u8> = Array3::zeros((200, 250, 3)); // 200x250 RGB
        reader.read_exact(array.as_slice_mut().unwrap()).unwrap();
        check_gradient(&array);
    }

    #[test]
    fn local_bin_file() {
        let local = DataBlock {
            location: Location::Path { path: "tests/files/gradient.bin".into(), index_id: None },
            byte_order: ByteOrder::Little, // doesn't matter, since we're using raw_bytes
            checksum: None,
            compression: None,
        };
        let ctx = Context::distributed("tests/files/");
        let mut reader = local.location.raw_bytes(&ctx).unwrap();
        let mut array: Array3<u8> = Array3::zeros((200, 250, 3)); // 200x250 RGB
        reader.read_exact(array.as_slice_mut().unwrap()).unwrap();
        check_gradient(&array);

        let relative = DataBlock {
            location: Location::Path { path: "@header_dir/gradient.bin".into(), index_id: None },
            ..local
        };
        let mut reader = relative.location.raw_bytes(&ctx).unwrap();
        reader.read_exact(array.as_slice_mut().unwrap()).unwrap();
        check_gradient(&array);
    }

    #[cfg(all(feature = "remote-http", not(docsrs)))]
    #[test]
    fn http_bin_file() {
        use url::Host;

        let http = DataBlock {
            location: Location::Url { url: "https://github.com/wrenby/xisf/raw/main/tests/files/gradient.bin".try_into().unwrap(), index_id: None },
            byte_order: ByteOrder::Little, // doesn't matter, since we're using raw_bytes
            checksum: None,
            compression: None,
        };
        let mut ctx = Context::distributed("tests/files/");
        let untrusted = http.location.raw_bytes(&ctx).err().unwrap();
        assert_eq!(untrusted.current_context(), &ReadDataBlockError::UntrustedHost(Host::Domain("github.com".into())));

        ctx.trust_host(Host::Domain("github.com".into()));
        let mut reader = http.location.raw_bytes(&ctx).unwrap();
        let mut array: Array3<u8> = Array3::zeros((200, 250, 3)); // 200x250 RGB
        reader.read_exact(array.as_slice_mut().unwrap()).unwrap();
        check_gradient(&array);
    }

    #[cfg(all(feature = "remote-ftp", not(docsrs)))]
    #[test]
    fn ftp_bin_file() {
        use testcontainers::{core::WaitFor, clients::Cli, images::generic::GenericImage, RunnableImage};
        let mut server: RunnableImage<_> = GenericImage::new("delfer/alpine-ftp-server", "latest")
            .with_env_var("USERS", "computer|deactivate_iguana|/files")
            .with_wait_for(WaitFor::message_on_stderr("passwd: password for computer changed by root"))
            .into();
        server = server.with_mapped_port((2121, 21))
            .with_volume(("./tests/files", "/files"));

        for pasv in 21000..=21010 {
            server = server.with_mapped_port((pasv, pasv));
        }

        let docker = Cli::docker();
        let container = docker.run(server);

        let ftp = DataBlock {
            location: Location::Url { url: "ftp://computer:deactivate_iguana@localhost:2121/files/gradient.bin".try_into().unwrap(), index_id: None },
            byte_order: ByteOrder::Little, // doesn't matter, since we're using raw_bytes
            checksum: None,
            compression: None,
        };
        let ctx = Context::distributed("tests/files/");
        let mut reader = ftp.location.raw_bytes(&ctx).unwrap();
        let mut array: Array3<u8> = Array3::zeros((200, 250, 3)); // 200x250 RGB
        reader.read_exact(array.as_slice_mut().unwrap()).unwrap();
        check_gradient(&array);

        container.stop();
    }

    #[test]
    fn zlib() {
        let file = File::open("tests/files/gradient.bin.zlib").unwrap();
        let size = file.metadata().unwrap().len();
        let zlib = DataBlock {
            location: Location::Attachment { position: 0, size },
            byte_order: ByteOrder::Little, // doesn't matter, since we're using raw_bytes
            checksum: None,
            compression: Some(Compression {
                algorithm: CompressionAlgorithm::Zlib,
                sub_blocks: SubBlocks(vec![(u64::MAX, GRADIENT_SIZE)]),
                byte_shuffling: None,
            }),
        };
        let ctx = Context::monolithic(BufReader::new(file));
        let mut reader = zlib.decompressed_bytes(&ctx).unwrap();
        let mut array: Array3<u8> = Array3::zeros((200, 250, 3)); // 200x250 RGB
        reader.read_exact(array.as_slice_mut().unwrap()).unwrap();
        check_gradient(&array);
    }

    #[test]
    fn lz4() {
        let file = File::open("tests/files/gradient.bin.lz4").unwrap();
        let size = file.metadata().unwrap().len();
        let lz4 = DataBlock {
            location: Location::Attachment { position: 0, size },
            byte_order: ByteOrder::Little, // doesn't matter, since we're using raw_bytes
            checksum: None,
            compression: Some(Compression {
                algorithm: CompressionAlgorithm::Lz4,
                sub_blocks: SubBlocks(vec![(u64::MAX, GRADIENT_SIZE)]),
                byte_shuffling: None,
            }),
        };
        let ctx = Context::monolithic(BufReader::new(file));
        let mut reader = lz4.decompressed_bytes(&ctx).unwrap();
        let mut array: Array3<u8> = Array3::zeros((200, 250, 3)); // 200x250 RGB
        reader.read_exact(array.as_slice_mut().unwrap()).unwrap();
        check_gradient(&array);
    }

    #[test]
    fn zstd() {
        let file = File::open("tests/files/gradient.bin.zst").unwrap();
        let size = file.metadata().unwrap().len();
        let zstd = DataBlock {
            location: Location::Attachment { position: 0, size },
            byte_order: ByteOrder::Little, // doesn't matter, since we're using raw_bytes
            checksum: None,
            compression: Some(Compression {
                algorithm: CompressionAlgorithm::Zstd,
                sub_blocks: SubBlocks(vec![(u64::MAX, GRADIENT_SIZE)]),
                byte_shuffling: None,
            }),
        };
        let ctx = Context::monolithic(BufReader::new(file));
        let mut reader = zstd.decompressed_bytes(&ctx).unwrap();
        let mut array: Array3<u8> = Array3::zeros((200, 250, 3)); // 200x250 RGB
        reader.read_exact(array.as_slice_mut().unwrap()).unwrap();
        check_gradient(&array);
    }
}
