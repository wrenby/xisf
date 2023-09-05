use std::{
    error::Error,
    fmt::{Display, Formatter},
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParseValueError(pub &'static str);
impl Display for ParseValueError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Failed to parse {}", self.0))
    }
}
impl Error for ParseValueError {}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ReadFitsKeyError {
    KeyNotFound,
    InvalidFormat,
}
impl Display for ReadFitsKeyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to read FITS key: ")?;
        match self {
            &Self::KeyNotFound => f.write_str("Key not found in header"),
            &Self::InvalidFormat => f.write_str("Failed to parse key value"),
        }
    }
}
impl Error for ReadFitsKeyError {}

/// A failure to either parse or resolve a `<Reference>` XML element
///
/// Enum variants are listed in order of precedence, e.g. if the parser encounters the node
/// `<Reference uid="invalid uid" ref="invalid uid" />` (a recursive reference and an invalid uid)
/// attempting to resolve the reference will return `InvalidUid` instead of `Recursive`
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ReferenceError {
    MissingRef,
    InvalidUid,
    NotFound,
    Recursive,
    Ambiguous,
}
impl Display for ReferenceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Invalid Reference element")
    }
}
impl Error for ReferenceError {}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ParseNodeErrorKind {
    InvalidReference,
    MissingAttr,
    InvalidAttr,
    MissingChild,
    /// not used in a general case, only for when the child itself doesn't return its own ParseNodeError, such as
    /// the text nodes of embedded [`DataBlock`](crate::data_block::DataBlock)s
    InvalidChild,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ParseNodeError {
    pub tag: &'static str,
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


#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ReadDataBlockError;
impl Display for ReadDataBlockError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to read bytes from XISF data block")
    }
}
impl Error for ReadDataBlockError {}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ReadFileError;
impl Display for ReadFileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to read XISF file")
    }
}
impl Error for ReadFileError {}