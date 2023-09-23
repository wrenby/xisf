//! XISF property parsing
//!
//! [Images](crate::image::Image) and [XISF root elements](crate::XISF) may have an arbitrary number of
//! "XISF properties": an association of a locally-unique ID with a type, value, format specifier, and comment.
//! Format specifiers are currently not supported.

use std::{collections::HashMap, fmt::Display, str::FromStr};

use error_stack::{report, Result, ResultExt, Report};
use libxml::{readonly::RoNode, tree::NodeType};
use num_complex::Complex;
use strum::Display;
use time::{OffsetDateTime, format_description::well_known::Iso8601};

use crate::{
    data_block::{Context, DataBlock},
    error::{ParseValueError, ParseNodeError, ParseNodeErrorKind::{self, *}, ReadPropertyError},
    reference::is_valid_id
};

/// A container for XISF properties
///
/// Reused across [`Image`](crate::image::Image) and [`XISF`](crate::XISF)
#[derive(Clone, Debug)]
pub struct Properties(HashMap<String, PropertyContent>);
impl Properties {
    /// Creates a new XISF property list
    pub(crate) fn new(map: HashMap<String, PropertyContent>) -> Self {
        Self(map)
    }

    /// Returns true iff an XISF property is present with the given ID
    pub fn contains(&self, id: impl AsRef<str>) -> bool {
        self.0.contains_key(id.as_ref())
    }

    /// Attempts to parse an XISF property with the given ID as type T
    ///
    /// To read a value and comment pair, use the pattern `let (value, comment) = properties.parse_property("ID", &xisf)?;`
    pub fn parse<T: FromProperty>(&self, id: impl AsRef<str>, ctx: &Context) -> Result<T, ReadPropertyError> {
        let content = self.0.get(id.as_ref())
            .ok_or(report!(ReadPropertyError::NotFound))?;
        T::from_property(&content, ctx)
            .change_context(ReadPropertyError::InvalidFormat)
    }

    /// Returns the raw content of the XISF property matching the given ID`
    pub fn raw(&self, id: impl AsRef<str>) -> Option<&PropertyContent> {
        self.0.get(id.as_ref())
    }

    /// Iterates through all XISF properties as (id, content) tuples,
    /// in the order they appear in file, returned as raw unparsed strings/data blocks.
    pub fn all_raw(&self) -> impl Iterator<Item = (&String, &PropertyContent)> {
        self.0.iter()
    }
}

/// A data type associated with an XISF property
// TODO: find a better representation for this -- need to store vector length and matrix width/height, and don't want to duplicate a ton of stuff to do it
#[derive(Clone, Copy, Debug, Display, PartialEq, Eq)]
pub enum NumericType {
    /// A scalar 8-bit signed integer
    Int8,
    /// A scalar 8-bit unsigned integer
    UInt8,
    /// A scalar 16-bit signed integer
    Int16,
    /// A scalar 16-bit unsigned integer
    UInt16,
    /// A scalar 32-bit signed integer
    Int32,
    /// A scalar 32-bit unsigned integer
    UInt32,
    /// A scalar 64-bit signed integer
    Int64,
    /// A scalar 64-bit unsigned integer
    UInt64,
    /// A scalar 128-bit signed integer
    Int128,
    /// A scalar 128-bit unsigned integer
    UInt128,
    /// A scalar 32-bit floating point real number
    Float32,
    /// A scalar 64-bit floating point real number
    Float64,
    /// A scalar 128-bit floating point real number
    Float128,
    /// A complex number with 32-bit floating point parts
    Complex32,
    /// A complex number with 64-bit floating point parts
    Complex64,
    /// A complex number with 128-bit floating point parts
    Complex128,
}
impl NumericType {
    fn prefix(&self) -> &'static str {
        match self {
            NumericType::Int8 => "I8",
            NumericType::UInt8 => "UI8",
            NumericType::Int16 => "I16",
            NumericType::UInt16 => "UI16",
            NumericType::Int32 => "I32",
            NumericType::UInt32 => "UI32",
            NumericType::Int64 => "I64",
            NumericType::UInt64 => "UI64",
            NumericType::Int128 => "I128",
            NumericType::UInt128 => "UI128",
            NumericType::Float32 => "F32",
            NumericType::Float64 => "F64",
            NumericType::Float128 => "F128",
            NumericType::Complex32 => "C32",
            NumericType::Complex64 => "C64",
            NumericType::Complex128 => "C128",
        }
    }
}


/// What data type the property expects to be parsed into
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PropertyType {
    /// A boolean (true or false)
    Boolean,
    /// An integer, real, or complex number
    Number(NumericType),
    /// A string of Unicode characters
    String,
    /// A date + time + UTC offset, conforming to ISO-8601
    TimePoint,
    /// A vector (1D array) of numbers
    Vector {
        /// The type of number contained in the vector
        r#type: NumericType,
        /// The dimensionality / number of elements in the vector
        len: usize
    },
    /// A matrix (2D array) of numbers
    Matrix {
        /// The type of number contained in the matrix
        r#type: NumericType,
        /// The number of rows in the matrix
        rows: usize,
        /// The number of columns in the matrix
        columns: usize
    },
}
impl Display for PropertyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean => f.write_str("Boolean"),
            Self::Number(n) => n.fmt(f),
            Self::String => f.write_str("String"),
            Self::TimePoint => f.write_str("TimePoint"),
            Self::Vector { r#type, .. } => f.write_fmt(format_args!("{}Vector", r#type.prefix())),
            Self::Matrix { r#type, .. } => f.write_fmt(format_args!("{}Matrix", r#type.prefix())),
        }
    }
}

#[derive(PartialEq)]
enum PropertyTypeAttr {
    Boolean,
    Number(NumericType),
    String,
    TimePoint,
    Vector(NumericType),
    Matrix(NumericType),
}
impl FromStr for PropertyTypeAttr {
    type Err = Report<ParseValueError>;

    fn from_str(s: &str) -> Result<Self, ParseValueError> {
        match s {
            "Boolean" => Ok(Self::Boolean),

            "Int8" => Ok(Self::Number(NumericType::Int8)),
            "UInt8" | "Byte" => Ok(Self::Number(NumericType::UInt8)),
            "Int16" | "Short" => Ok(Self::Number(NumericType::Int16)),
            "UInt16" | "UShort" => Ok(Self::Number(NumericType::UInt16)),
            "Int32" | "Int" => Ok(Self::Number(NumericType::Int32)),
            "UInt32" | "UInt" => Ok(Self::Number(NumericType::UInt32)),
            "Int64" => Ok(Self::Number(NumericType::Int64)),
            "UInt64" => Ok(Self::Number(NumericType::UInt64)),
            "Int128" => Ok(Self::Number(NumericType::Int128)),
            "UInt128" => Ok(Self::Number(NumericType::UInt128)),
            "Float32" | "Float" => Ok(Self::Number(NumericType::Float32)),
            "Float64" | "Double" => Ok(Self::Number(NumericType::Float64)),
            "Float128" | "Quad" => Ok(Self::Number(NumericType::Float128)),
            "Complex32" => Ok(Self::Number(NumericType::Complex32)),
            "Complex64" | "Complex" => Ok(Self::Number(NumericType::Complex64)),
            "Complex128" => Ok(Self::Number(NumericType::Complex128)),

            "String" => Ok(Self::String),
            "TimePoint" => Ok(Self::TimePoint),

            "I8Vector" => Ok(Self::Vector(NumericType::Int8)),
            "UI8Vector" | "ByteArray" => Ok(Self::Vector(NumericType::UInt8)),
            "I16Vector" => Ok(Self::Vector(NumericType::Int16)),
            "UI16Vector" => Ok(Self::Vector(NumericType::UInt16)),
            "I32Vector" | "IVector" => Ok(Self::Vector(NumericType::Int32)),
            "UI32Vector" | "UIVector" => Ok(Self::Vector(NumericType::UInt32)),
            "I64Vector" => Ok(Self::Vector(NumericType::Int64)),
            "UI64Vector" => Ok(Self::Vector(NumericType::UInt64)),
            "I128Vector" => Ok(Self::Vector(NumericType::Int128)),
            "UI128Vector" => Ok(Self::Vector(NumericType::UInt128)),
            "F32Vector" => Ok(Self::Vector(NumericType::Float32)),
            "F64Vector" | "Vector" => Ok(Self::Vector(NumericType::Float64)),
            "F128Vector" => Ok(Self::Vector(NumericType::Float128)),
            "C32Vector" => Ok(Self::Vector(NumericType::Complex32)),
            "C64Vector" => Ok(Self::Vector(NumericType::Complex64)),
            "C128Vector" => Ok(Self::Vector(NumericType::Complex128)),

            "I8Matrix" => Ok(Self::Matrix(NumericType::Int8)),
            "UI8Matrix" | "ByteMatrix" => Ok(Self::Matrix(NumericType::UInt8)),
            "I16Matrix" => Ok(Self::Matrix(NumericType::Int16)),
            "UI16Matrix" => Ok(Self::Matrix(NumericType::UInt16)),
            "I32Matrix" | "IMatrix" => Ok(Self::Matrix(NumericType::Int32)),
            "UI32Matrix" | "UIMatrix" => Ok(Self::Matrix(NumericType::UInt32)),
            "I64Matrix" => Ok(Self::Matrix(NumericType::Int64)),
            "UI64Matrix" => Ok(Self::Matrix(NumericType::UInt64)),
            "I128Matrix" => Ok(Self::Matrix(NumericType::Int128)),
            "UI128Matrix" => Ok(Self::Matrix(NumericType::UInt128)),
            "F32Matrix" => Ok(Self::Matrix(NumericType::Float32)),
            "F64Matrix" | "Matrix" => Ok(Self::Matrix(NumericType::Float64)),
            "F128Matrix" => Ok(Self::Matrix(NumericType::Float128)),
            "C32Matrix" => Ok(Self::Matrix(NumericType::Complex32)),
            "C64Matrix" => Ok(Self::Matrix(NumericType::Complex64)),
            "C128Matrix" => Ok(Self::Matrix(NumericType::Complex128)),

            bad => Err(ParseValueError("XISF Property Type"))
                .attach_printable(format!("Invalid type: {bad}"))
        }
    }
}
impl PropertyTypeAttr {
    pub(crate) fn vector_or_matrix(&self) -> bool {
        match self {
            Self::Matrix(_) | Self::Vector(_) => true,
            _ => false,
        }
    }
}

fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}
const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("Property", kind)
}

#[derive(Clone, Debug)]
pub(crate) struct Property {
    pub(crate) id: String,
    pub(crate) content: PropertyContent,
}
impl Property {
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, ParseNodeError> {
        let _span_guard = tracing::debug_span!("Property");
        let mut attrs = node.get_attributes();
        let children = node.get_child_nodes();

        let id = attrs.remove("id")
            .ok_or(context(MissingAttr))
            .attach_printable("Missing id attribute")?;

        // an XISF id can have an arbitrary number of namespaces, separated by colons, before the final namespace-local id
        // for example: Instrument:Telescope:Aperture or Observation:Object:Name
        for part in id.split(":") {
            if !is_valid_id(part) {
                return Err(report(InvalidAttr)).attach_printable("Invalid id attribute: ")
            }
        }

        let type_attr = attrs.remove("type")
            .ok_or(context(MissingAttr))
            .attach_printable("Missing type attribute")?
            .parse::<PropertyTypeAttr>()
            .change_context(context(InvalidAttr))
            .attach_printable("Failed to parse type attribute")?;

        let comment = attrs.remove("comment");

        // TODO: format

        let data_block = DataBlock::parse_node(node, "Property", &mut attrs)?;

        //
        macro_rules! warn_remaining {
            (attrs) => {
                for remaining in attrs.into_iter() {
                    tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
                }
            };
            (attrs, children) => {
                warn_remaining!(attrs);
                for child in children {
                    tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
                }
            };
        }

        // TODO: refactor into match statement
        if type_attr == PropertyTypeAttr::String {
            let r#type = PropertyType::String;
            // string properties can either be in a data block or a child text node
            if let Some(block) = data_block {
                warn_remaining!(attrs, children);
                Ok(Self {
                    id,
                    content: PropertyContent {
                        r#type,
                        value: PropertyValue::DataBlock(block),
                        comment,
                    }
                })
            } else {
                match children.as_slice() {
                    [] => Err(report(MissingChild)).attach_printable("Missing child text node: required for inline data blocks"),
                    [text] if text.get_type() == Some(NodeType::TextNode) => {
                        warn_remaining!(attrs);
                        Ok(Self {
                            id,
                            content: PropertyContent {
                                r#type,
                                value: PropertyValue::Plaintext(text.get_content()),
                                comment,
                            },
                        })
                    },
                    _other => Err(report(InvalidChild)).attach_printable("String properties are not permitted to have non-text child nodes"),
                }
            }
        } else if type_attr.vector_or_matrix() {
            let r#type = match type_attr {
                PropertyTypeAttr::Vector(inner) => {
                    let len = attrs.remove("length")
                        .ok_or(context(MissingAttr))
                        .attach_printable("Missing length attribute")?
                        .trim()
                        .parse::<usize>()
                        .change_context(context(InvalidAttr))
                        .attach_printable("Failed to parse length attribute")?;

                    PropertyType::Vector {
                        r#type: inner,
                        len,
                    }
                },
                PropertyTypeAttr::Matrix(inner) => {
                    let rows = attrs.remove("rows")
                        .ok_or(context(MissingAttr))
                        .attach_printable("Missing rows attribute")?
                        .trim()
                        .parse::<usize>()
                        .change_context(context(InvalidAttr))
                        .attach_printable("Failed to parse rows attribute")?;

                    let columns = attrs.remove("columns")
                        .ok_or(context(MissingAttr))
                        .attach_printable("Missing columns attribute")?
                        .trim()
                        .parse::<usize>()
                        .change_context(context(InvalidAttr))
                        .attach_printable("Failed to parse columns attribute")?;

                    PropertyType::Matrix {
                        r#type: inner,
                        rows,
                        columns,
                    }
                },
                _ => unreachable!()
            };
            if let Some(block) = data_block {
                warn_remaining!(attrs, children);
                Ok(Self {
                    id,
                    content: PropertyContent {
                        r#type,
                        value: PropertyValue::DataBlock(block),
                        comment,
                    }
                })
            } else {
                Err(context(InvalidAttr)).attach_printable(format!("Properties of type {} must have a data block", r#type))
            }
        } else {
            let r#type = match type_attr {
                PropertyTypeAttr::Boolean => PropertyType::Boolean,
                PropertyTypeAttr::Number(inner) => PropertyType::Number(inner),
                PropertyTypeAttr::TimePoint => PropertyType::TimePoint,
                _ => unreachable!()
            };
            if data_block.is_none() {
                if let Some(val) = attrs.remove("value") {
                    warn_remaining!(attrs, children);
                    Ok(Self {
                        id,
                        content: PropertyContent {
                            r#type,
                            value: PropertyValue::Plaintext(val),
                            comment,
                        }
                    })
                } else {
                    Err(context(MissingAttr)).attach_printable(format!("Missing value attribute"))
                }
            } else {
                Err(context(InvalidAttr)).attach_printable(format!("Properties of type {} cannot have a data block", r#type))
            }
        }

    }
}

/// The value of this property
///
/// Since properties are parsed lazily,
///
/// For simple (atomic) types such as [numbers](PropertyType::Number) or [TimePoint](PropertyType::TimePoint),
/// this will come from a `value` attribute in the XML node -- with the exception of [String](PropertyType::String)
/// properties, which have their own rules -- and will be stored in the `Plaintext` variant.
///
/// For compound types such as vectors and matrices, this will come from a [data block](crate::data_block::DataBlock)
#[derive(Clone, Debug, PartialEq)]
pub enum PropertyValue {
    /// The value of this property is stored in the XML node itself
    ///
    /// This may come from a `value` attribute or from a text node, depending on the property type
    Plaintext(String),
    /// The value of this property is stored in a [data block](crate::data_block::DataBlock)
    DataBlock(DataBlock),
}

/// All components of an XISF property other than its ID
#[derive(Clone, Debug)]
pub struct PropertyContent {
    /// The type this property expects to be parsed as
    ///
    /// `type` is a reserved word in Rust, so `content.type` will fail to compile. To access this field, use `content.r#type`.
    pub r#type: PropertyType,
    /// The value of this property
    ///
    /// Properties are parsed lazily, so this may contain a value incompatible with [Self::type].
    /// For example, vectors and matrices must be serialized in data blocks, but the XISF reader
    /// doesn't enforce that at load-time.
    pub value: PropertyValue,
    /// An optional comment with arbitrary unicode explaining the use of or context for this property
    pub comment: Option<String>,
}

/// Convert an XISF property to a concrete type
pub trait FromProperty: Sized {
    /// Attempt to parse an XISF property as `Self`
    fn from_property(prop: &PropertyContent, ctx: &Context) -> Result<Self, ParseValueError>;
}

impl FromProperty for bool {
    fn from_property(prop: &PropertyContent, _ctx: &Context) -> Result<Self, ParseValueError> {
        const CONTEXT: ParseValueError = ParseValueError("XISF Property as bool");
        if prop.r#type == PropertyType::Boolean {
            match &prop.value {
                PropertyValue::Plaintext(text) if text == "0" => Ok(false),
                PropertyValue::Plaintext(text) if text == "1" => Ok(true),
                PropertyValue::Plaintext(bad) => Err(report!(CONTEXT))
                    .attach_printable(format!("Expected one of [0, 1], found {bad}")),
                PropertyValue::DataBlock(_) => Err(report!(CONTEXT))
                    .attach_printable("Scalar properties cannot be serialized as a data block")
            }
        } else {
            Err(report!(CONTEXT))
                .attach_printable(format!("Incorrect property type: found {}", prop.r#type))
        }
    }
}

macro_rules! from_property_scalar {
    ($variant:ident, $t:ty) => {
        impl FromProperty for $t {
            fn from_property(prop: &PropertyContent, _ctx: &Context) -> Result<Self, ParseValueError> {
                const CONTEXT: ParseValueError = ParseValueError(concat!("XISF Property key as ", stringify!($t)));
                if prop.r#type == PropertyType::Number(NumericType::$variant) {
                    if let PropertyValue::Plaintext(text) = &prop.value {
                        Ok(text.trim().parse::<$t>().change_context(CONTEXT)?)
                    } else {
                        Err(report!(CONTEXT)).attach_printable("Scalar properties cannot be serialized as a data block")
                    }
                } else {
                    Err(report!(CONTEXT))
                        .attach_printable(format!("Incorrect property type: found {}", prop.r#type))
                }
            }
        }
    }
}

from_property_scalar!(Int8, i8);
from_property_scalar!(UInt8, u8);
from_property_scalar!(Int16, i16);
from_property_scalar!(UInt16, u16);
from_property_scalar!(Int32, i32);
from_property_scalar!(UInt32, u32);
from_property_scalar!(Int64, i64);
from_property_scalar!(UInt64, u64);
from_property_scalar!(Int128, i128);
from_property_scalar!(UInt128, u128);
from_property_scalar!(Float32, f32);
from_property_scalar!(Float64, f64);
// TODO: from_property_scalar!(Float128, f128)

macro_rules! from_property_complex {
    ($variant:ident, $t:ty) => {
        impl FromProperty for Complex<$t> {
            fn from_property(prop: &PropertyContent, _ctx: &Context) -> Result<Self, ParseValueError> {
                const CONTEXT: ParseValueError = ParseValueError(concat!("XISF Property key as Complex<", stringify!($t), '>'));
                if prop.r#type == PropertyType::Number(NumericType::$variant) {
                    if let PropertyValue::Plaintext(text) = &prop.value {
                        let val = text.trim();
                        if val.starts_with('(') && val.ends_with(')') {
                            if let Some((re, im)) = val[1..val.len()-1].split_once(',') {
                                Ok(Complex::<$t>::new(
                                    re.trim().parse::<$t>()
                                        .change_context(CONTEXT)
                                        .attach_printable(concat!("Failed to parse real part as ", stringify!($t)))?,
                                    im.trim().parse::<$t>()
                                        .change_context(CONTEXT)
                                        .attach_printable(concat!("Failed to parse imaginary part as ", stringify!($t)))?,
                                ))
                            } else {
                                Err(report!(CONTEXT)).attach_printable("Missing comma")
                            }
                        } else {
                            Err(report!(CONTEXT)).attach_printable("Missing parentheses")
                        }
                    } else {
                        Err(report!(CONTEXT)).attach_printable("Complex properties cannot be serialized as a data block")
                    }
                } else {
                    Err(report!(CONTEXT))
                        .attach_printable(format!("Incorrect property type: found {}", prop.r#type))
                }
            }
        }
    }
}

from_property_complex!(Complex32, f32);
from_property_complex!(Complex64, f64);
// TODO: from_property_complex!(Complex64, f128);

impl FromProperty for String {
    fn from_property(prop: &PropertyContent, ctx: &Context) -> Result<Self, ParseValueError> {
        const CONTEXT: ParseValueError = ParseValueError("XISF Property as String");
        if prop.r#type == PropertyType::String {
            match &prop.value {
                PropertyValue::Plaintext(text) => Ok(text.clone()),
                PropertyValue::DataBlock(block) => {
                    block.verify_checksum(ctx)
                        .change_context(CONTEXT)?;
                    let mut reader = block.decompressed_bytes(ctx)
                        .change_context(CONTEXT)?;

                    let mut string = String::new();
                    reader.read_to_string(&mut string)
                        .change_context(CONTEXT)?;
                    Ok(string)
                }
            }
        } else {
            Err(report!(CONTEXT))
                .attach_printable(format!("Incorrect property type: found {}", prop.r#type))
        }
    }
}

impl FromProperty for OffsetDateTime {
    fn from_property(prop: &PropertyContent, _ctx: &Context) -> Result<Self, ParseValueError> {
        const CONTEXT: ParseValueError = ParseValueError(concat!("XISF Property key as ", stringify!($t)));
        if prop.r#type == PropertyType::TimePoint {
            if let PropertyValue::Plaintext(text) = &prop.value {
                Ok(OffsetDateTime::parse(text.trim(), &Iso8601::PARSING)
                    .change_context(CONTEXT)?)
            } else {
                Err(report!(CONTEXT)).attach_printable("Scalar properties cannot be serialized as a data block")
            }
        } else {
            Err(report!(CONTEXT))
                .attach_printable(format!("Incorrect property type: found {}", prop.r#type))
        }
    }
}

// TODO: this is recursive
impl<T: FromProperty> FromProperty for (T, Option<String>) {
    fn from_property(prop: &PropertyContent, ctx: &Context) -> Result<Self, ParseValueError> {
        Ok((T::from_property(prop, ctx)?, prop.comment.clone()))
    }
}
