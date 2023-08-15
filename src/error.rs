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
        f.write_str("Failed to read XISF image data")
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