use std::{
    any::type_name,
    num::NonZeroU8,
    path::PathBuf,
};

use error_stack::{Report, IntoReport, ResultExt, report};
use libxml::{readonly::RoNode, tree::NodeType};
use parse_int::parse as parse_auto_radix;
use strum::{EnumString, Display};
use url::Url;
use crate::error::{ParseValueError, ParseNodeError};

#[derive(Clone, Copy, Debug, Default, Display, EnumString)]
pub enum Encoding {
    #[default]
    Base64,
    Hex, // hexadecimal (base 16), must be serialized lowercase
}

#[derive(Clone, Debug)]
pub enum DataBlockLocation {
    Inline {
        // data is encoded in a child text node
        encoding: Encoding,
        text: String, // ! may still include whitespace, which must be ignored when decoding
    },
    Embedded {
        // data is encoded in a child <Data> node
        encoding: Encoding,
        text: String, // ! may still include whitespace, which must be ignored when decoding
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
impl DataBlockLocation {
    // returns Ok(Some(_)) if a data block location was successfully parsed
    // returns Ok(None) if there is no location attribute
    // returns Err(_) if there was an error parsing the data block location
    pub(crate) fn from_node(node: &RoNode, context: ParseNodeError) -> Result<Option<Self>, Report<ParseNodeError>> {
        if let Some(attr) = node.get_attribute("location") {
            match attr.split(":").collect::<Vec<_>>().as_slice() {
                &["inline", encoding] => {
                    let encoding = encoding.parse::<Encoding>()
                        .into_report()
                        .change_context(context)
                        .attach_printable("Invalid location attribute: failed to parse inline encoding")?;

                    match node.get_child_nodes().as_slice() {
                        [] => Err(report!(context)).attach_printable("Missing child text node: required for inline data blocks"),
                        [text] if text.get_type() == Some(NodeType::TextNode) => Ok(Some(
                            Self::Inline { encoding, text: text.get_content() }
                        )),
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

                                match node.get_child_nodes().as_slice() {
                                    [] => Err(report!(context)).attach_printable("Embedded <Data> node missing child text node"),
                                    [text] if text.get_type() == Some(NodeType::TextNode) => Ok(Some(
                                        Self::Embedded { encoding, text: text.get_content() }
                                    )),
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
                    Ok(Some(Self::Path {
                        // the slice indexing trims "path(" from the front and ")" from the end
                        path: PathBuf::from(&path[5..path.len()-1]),
                        index_id: None,
                    }))
                },
                &[path, index_id] if path.starts_with("path(") && path.ends_with(")") => {
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
    pub fn is_remote(&self) -> bool {
        // TODO: should local files count as remote? there's a chance of a local file leaking your IP if it's mounted via fuse or something
        match self {
            Self::Url { url, .. } if url.scheme() != "file" => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Display, EnumString)]
pub enum ByteOrder {
    #[strum(serialize = "big")]
    Big,
    #[default]
    #[strum(serialize = "little")]
    Little,
}

#[derive(Clone, Copy, Debug, Display, EnumString)]
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

#[derive(Clone, Copy, Debug, Display, EnumString)]
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

#[derive(Clone, Copy, Debug, Default)]
pub enum CompressionLevel {
    #[default]
    Auto,
    Value(NonZeroU8), // minimum: 1, maximum: 100
}
impl CompressionLevel {
    pub fn new(level: u8) -> Result<Self, Report<ParseValueError>> {
        match level {
            0 => Ok(Self::Auto),
            (1..=100) => Ok(Self::Value(NonZeroU8::new(level).unwrap())),
            _ => Err(ParseValueError(level.to_string(), type_name::<Self>()))
                .into_report()
                .attach_printable("Must be between 0 and 100")
        }
    }
}