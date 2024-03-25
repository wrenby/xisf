//! All of the error types returned by API functions

use std::{
    error::Error,
    fmt::{Display, Formatter},
};

use url::{Host, Url};

/// A failure to parse a simple value such as a primitive type or an enum, usually from an XML attribute
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParseValueError(pub &'static str);
impl Display for ParseValueError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Failed to parse {}", self.0))
    }
}
impl Error for ParseValueError {}

/// A failure to parse a [FITS key](crate::image::FitsKeyContent) from the header
///
/// Counterpart to [`ReadPropertyError`]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ReadFitsRecordError {
    /// Could not find a FITS key with the given name
    NotFound,
    /// Failed to parse the FITS key into the requested type
    InvalidFormat,
}
impl Display for ReadFitsRecordError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to read FITS key: ")?;
        match self {
            &Self::NotFound => f.write_str("Key not found in header"),
            &Self::InvalidFormat => f.write_str("Failed to parse key value"),
        }
    }
}
impl Error for ReadFitsRecordError {}

/// A Failure to parse an [XISF Property](crate::property::PropertyContent)
///
/// Counterpart to [`ReadFitsKeyError`]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ReadPropertyError {
    /// Could not find an XISF property with the given id
    NotFound,
    /// Failed to parse the property into the requested type
    InvalidFormat,
}
impl Display for ReadPropertyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to read XISF property: ")?;
        match self {
            &Self::NotFound => f.write_str("ID not found"),
            &Self::InvalidFormat => f.write_str("Failed to parse value"),
        }
    }
}
impl Error for ReadPropertyError {}

/// A failure to either parse or resolve a `<Reference>` XML element
///
/// Enum variants are listed in order of precedence, e.g. if the parser encounters the node
/// `<Reference uid="invalid uid" ref="invalid uid" />` (a recursive reference and an invalid uid)
/// attempting to resolve the reference will return `InvalidUid` instead of `Recursive`
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ReferenceError {
    /// The `ref` attribute is missing, so there isn't anything to search for
    MissingRef,
    /// The `ref` attribute contains an invalid uid
    ///
    /// Valid uids satisfy the regex `[_a-zA-Z][_a-zA-Z0-9]*`
    InvalidUid,
    /// No elements found with the requested uid
    NotFound,
    /// Reference to a reference
    Recursive,
    /// More than one element was found with the requested uid
    Ambiguous,
}
impl Display for ReferenceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Invalid Reference element")
    }
}
impl Error for ReferenceError {}

///
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ParseNodeErrorKind {
    /// Node
    InvalidReference,
    /// This element requires an attribute that was not present
    MissingAttr,
    /// An attribute on this element parsing or validation
    InvalidAttr,
    /// This element requires a child node that was not present
    MissingChild,
    /// not used in a general case, only for when the child itself doesn't return its own ParseNodeError, such as
    /// the text nodes of embedded [`DataBlock`](crate::data_block::DataBlock)s
    InvalidChild,
}

/// A failure to parse a [Core Element](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__XISF_Core_Elements__)
/// from the XML document making up an XISF header
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParseNodeError {
    /// The XML tag name of the element that could not be parsed
    ///
    /// E.g. if an `<Image ...>` element could not be parsed, `tag` will be "Image"
    pub tag: &'static str,
    /// What went wrong while trying to parse the element
    pub kind: ParseNodeErrorKind,
}
impl ParseNodeError {
    pub(crate) const fn new(tag: &'static str, kind: ParseNodeErrorKind) -> Self {
        Self {
            tag,
            kind,
        }
    }
}
impl Display for ParseNodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Failed to parse <{}> node in XML header: ", self.tag))?;

        use ParseNodeErrorKind::*;
        match self.kind {
            InvalidReference => f.write_str("Invalid child <Reference> node"),
            MissingAttr => f.write_str("Missing attribute"),
            InvalidAttr => f.write_str("Invalid Attribute"),
            MissingChild => f.write_str("Missing child node"),
            InvalidChild => f.write_str("Invalid child node"),
        }
    }
}
impl Error for ParseNodeError {}

/// A failure to read bytes from a [`DataBlock`](crate::data_block::DataBlock)
#[derive(Clone, Debug, PartialEq)]
pub enum ReadDataBlockError {
    /// Something went wrong while interacting with the underlying file or stream
    IoError,
    /// The data block location is not supported for the current file
    ///
    /// For example, trying to access a remote data block in a monolithic file,
    /// or trying to access an attached data block in a distributed file.
    UnsupportedLocation,
    /// The checksum failed verification
    DifferentChecksum,
    ///
    BadByteShuffleItemSize,
    /// Failed to read an attached data block because the file is in use by another data block
    ///
    /// I'm pretty sure this is impossible to get with safe rust since [`Context`](crate::data_block::Context) doesn't `impl Send + Sync`,
    /// and the file handle is released by the time the public API call finishes, but it's here just in case I'm wrong.
    FileInUse,
    /// Failed to read an inline/embedded data block
    BadTextEncoding,
    /// The host is not in the context's
    ///
    /// Ask the user if they trust the provided host, and if yes,
    /// add it to the context with [`Context::trust_host`](crate::data_block::Context::trust_host), and try to read the block again
    ///
    /// <div class="warning">
    /// This should not be done automatically, as a malicious server could permanently hang the reader.
    /// Always ask before adding a trusted host.
    /// </div>
    UntrustedHost(Host),
    /// Failed to authenticate with the remote host
    ///
    /// <div class="warning">
    /// Some servers may enforce a cooldown period between attempts, so it's best to be proactive in asking for credentials before connecting.
    /// Consider asking for credentials at the same time as when you ask the user to trust the host.
    /// </div>
    Unauthorized(Url),
    /// Current supported schemes: `ftp`, `http`, `https`, `file`
    UnsupportedScheme(String),
    /// The URL for this data block is missing a remote host
    MissingHost,
}
impl Display for ReadDataBlockError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to read XISF data block: ")?;
        match self {
            ReadDataBlockError::IoError => f.write_str("I/O Error"),
            ReadDataBlockError::UnsupportedLocation => f.write_str("Data block location not supported for file type"),
            ReadDataBlockError::DifferentChecksum => f.write_str("Checksum verification failed"),
            ReadDataBlockError::BadByteShuffleItemSize => f.write_str("Uncompressed size is not divisible by byte shuffling item size"),
            ReadDataBlockError::FileInUse => f.write_str("File handle is in use by another data block"),
            ReadDataBlockError::BadTextEncoding => f.write_str("Failed to decode embedded or inline data block"),
            ReadDataBlockError::UntrustedHost(host) => f.write_fmt(format_args!("Remote host {} is not trusted by user", host)),
            ReadDataBlockError::Unauthorized(url) => f.write_fmt(format_args!("Access to remote resource at {} is unauthorized", url)),
            ReadDataBlockError::UnsupportedScheme(scheme) => f.write_fmt(format_args!("URI scheme {} is not supported", scheme)),
            ReadDataBlockError::MissingHost => f.write_str("Remote data block URI is missing a host"),
        }
    }
}
impl Error for ReadDataBlockError {}

/// A failure to read an XISF file
///
/// Since data from XISF files are read lazily, a lack of `ReadFileError` does not guarantee that
/// all data blocks can be read, or all FITS keys/XISF properties can be parsed, for example.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ReadFileError;
impl Display for ReadFileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to read XISF file")
    }
}
impl Error for ReadFileError {}

/// A failure to convert a [`DynImageData`](crate::image::DynImageData)
/// enum to one of its [`ImageData`](crate::image::ImageData) variants
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DowncastDynImageError(pub &'static str);
impl Display for DowncastDynImageError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Failed to downcast DynImageData to type {}", self.0))
    }
}
impl Error for DowncastDynImageError {}
