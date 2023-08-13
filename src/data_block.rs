use std::{
    any::type_name,
    path::PathBuf,
};

use error_stack::{Report, IntoReport, ResultExt};
use url::Url;
use crate::error::ParseStringError;

pub enum Encoding {
    Base64,
    Hex, // hexadecimal (base 16), must be serialized lowercase
}

pub enum DataBlockLocation {
    InlineOrEmbedded {
        // data is encoded in a child text or <Data> node
        // makes no difference to us which it is, so store them identically
        encoding: Encoding,
        text: String, // ! still includes whitespace, which must be ignored
    },
    Attachment {
        // data is elsewhere in the file
        // ! only supported for monolithic XISF files
        offset: u64,
        size: u64,
    },
    Url {
        // data is stored remotely
        // ! only supported for distributed XISF files
        url: Url,
        index_id: u64,
    },
    Path {
        // data is store elsewhere on the filesystem
        // ! only supported for distributed XISF files
        path: PathBuf,
        index_id: u64,
    }
}
impl DataBlockLocation {
    pub(crate) fn is_remote(&self) -> bool {
        // TODO: should local files count as remote? there's a chance of a local file leaking your IP if it's mounted via fuse or something
        match self {
            Self::Url { url, .. } if url.scheme() != "file" => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ChecksumAlgorithm {
    Sha1,
    Sha256,
    Sha512,
    Sha3_256,
    Sha3_512,
}

#[derive(Clone, Copy, Debug)]
pub enum CompressionAlgorithm {
    Zlib,
    ZlibByteShuffling,
    Lz4,
    Lz4ByteShuffling,
    Lz4HC,
    Lz4HCByteShuffling,
}

#[derive(Clone, Copy, Debug)]
pub struct CompressionLevel(u32);
impl CompressionLevel {
    pub const AUTO: Self = CompressionLevel(0);
    pub const FAST: Self = CompressionLevel(1);
    pub const MAX: Self = CompressionLevel(100);
    pub fn new(level: u32) -> Result<Self, Report<ParseStringError>> {
        if (0..=100).contains(&level) {
            Ok(Self(level))
        } else {
            Err(ParseStringError(level.to_string(), type_name::<Self>()))
                .into_report()
                .attach_printable("Must be between 0 and 100")
        }
    }
}