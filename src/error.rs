use std::{
    error::Error,
    fmt::{Display, Formatter},
};

#[derive(Clone, Debug)]
pub struct ParseValueError(pub &'static str);
impl Display for ParseValueError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Failed to parse {}", self.0))
    }
}
impl Error for ParseValueError {}

#[derive(Clone, Debug)]
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

// TODO: get rid of this; replace it with a ParseNodeError
#[derive(Clone, Debug)]
pub struct ReferenceError;
impl Display for ReferenceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Invalid Reference element")
    }
}
impl Error for ReferenceError {}

#[derive(Clone, Copy, Debug)]
pub struct ParseNodeError(pub &'static str);
impl Display for ParseNodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Failed to parse <{}> node in XML header", self.0))
    }
}
impl Error for ParseNodeError {}


#[derive(Clone, Copy, Debug)]
pub struct ReadDataBlockError;
impl Display for ReadDataBlockError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to read bytes from XISF data block")
    }
}
impl Error for ReadDataBlockError {}

#[derive(Clone, Copy, Debug)]
pub struct ReadFileError;
impl Display for ReadFileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to read XISF file")
    }
}
impl Error for ReadFileError {}