//! XISF property parsing
//!
//! Nothing in here will be likely to be used directly, but it's exposed as a public module
//! as a means to
//!
//! See [`ImageBase::parse_property`](crate::image::ImageBase::parse_property),
//! [`XISF::parse_property`](crate::XISF::parse_property),
//! and [`XISF::parse_metadata`](crate::XISF::parse_metadata) for likely entry points

use error_stack::{report, Result, ResultExt, Report};
use libxml::{readonly::RoNode, tree::NodeType};
use num_complex::Complex;
use strum::{EnumString, Display};
use time::{OffsetDateTime, format_description::well_known::Iso8601};

use crate::{
    data_block::{Context, DataBlock},
    error::{ParseValueError, ParseNodeError, ParseNodeErrorKind::{self, *}},
    reference::is_valid_id
};

/// A data type associated with an XISF property
// TODO: find a better representation for this -- need to store vector length and matrix width/height, and don't want to duplicate a ton of stuff to do it
#[repr(u8)]
#[derive(Clone, Copy, Debug, Display, EnumString, PartialEq)]
pub enum PropertyType {
    /// A boolean (true or false)
    Boolean,
    /// A scalar 8-bit signed integer
    Int8,
    #[strum(serialize = "UInt8", serialize = "Byte")]
    /// A scalar 8-bit unsigned integer
    UInt8,
    #[strum(serialize = "Int16", serialize = "Short")]
    /// A scalar 16-bit signed integer
    Int16,
    #[strum(serialize = "UInt16", serialize = "UShort")]
    /// A scalar 16-bit unsigned integer
    UInt16,
    #[strum(serialize = "Int32", serialize = "Int")]
    /// A scalar 32-bit signed integer
    Int32,
    #[strum(serialize = "UInt32", serialize = "UInt")]
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
    #[strum(serialize = "Float32", serialize = "Float")]
    Float32,
    /// A scalar 64-bit floating point real number
    #[strum(serialize = "Float64", serialize = "Double")]
    Float64,
    /// A scalar 128-bit floating point real number
    #[strum(serialize = "Float128", serialize = "Quad")]
    Float128,
    /// A complex number with 32-bit floating point parts
    Complex32,
    /// A complex number with 64-bit floating point parts
    #[strum(serialize = "Complex64", serialize = "Complex")]
    Complex64,
    /// A complex number with 128-bit floating point parts
    Complex128,
    /// A string of Unicode characters
    String,
    /// A date + time + UTC offset, conforming to ISO-8601
    TimePoint,
    /// A vector of [`Int8`](Self::Int8) elements
    I8Vector,
    #[strum(serialize = "UI8Vector", serialize = "ByteArray")]
    /// A vector of [`UInt8`](Self::UInt8) elements
    UI8Vector,
    /// A vector of [`Int16`](Self::Int16) elements
    I16Vector,
    /// A vector of [`UInt16`](Self::UInt16) elements
    UI16Vector,
    #[strum(serialize = "I32Vector", serialize = "IVector")]
    /// A vector of [`Int32`](Self::Int32) elements
    I32Vector,
    #[strum(serialize = "UI32Vector", serialize = "UIVector")]
    /// A vector of [`UInt32`](Self::UInt32) elements
    UI32Vector,
    /// A vector of [`Int64`](Self::Int64) elements
    I64Vector,
    /// A vector of [`UInt64`](Self::UInt64) elements
    UI64Vector,
    /// A vector of [`Int128`](Self::Int128) elements
    I128Vector,
    /// A vector of [`UInt128`](Self::UInt128) elements
    UI128Vector,
    /// A vector of [`Float32`](Self::Float32) elements
    F32Vector,
    #[strum(serialize = "F64Vector", serialize = "Vector")]
    /// A vector of [`Float64`](Self::Float64) elements
    F64Vector,
    /// A vector of [`Float128`](Self::Float128) elements
    F128Vector,
    /// A vector of [`Complex32`](Self::Complex32) elements
    C32Vector,
    /// A vector of [`Complex64`](Self::Complex64) elements
    C64Vector,
    /// A vector of [`Complex128`](Self::Complex128) elements
    C128Vector,
    /// A matrix of [`Int8`](Self::Int8) elements
    I8Matrix,
    #[strum(serialize = "UI8Matrix", serialize = "ByteMatrix")]
    /// A matrix of [`UInt8`](Self::UInt8) elements
    UI8Matrix,
    /// A matrix of [`Int16`](Self::Int16) elements
    I16Matrix,
    /// A matrix of [`UInt16`](Self::UInt16) elements
    UI16Matrix,
    #[strum(serialize = "I32Matrix", serialize = "IMatrix")]
    /// A matrix of [`Int32`](Self::Int32) elements
    I32Matrix,
    #[strum(serialize = "UI32Matrix", serialize = "UIMatrix")]
    /// A matrix of [`UInt32`](Self::UInt32) elements
    UI32Matrix,
    /// A matrix of [`Int64`](Self::Int64) elements
    I64Matrix,
    /// A matrix of [`UInt64`](Self::UInt64) elements
    UI64Matrix,
    /// A matrix of [`Int128`](Self::Int128) elements
    I128Matrix,
    /// A matrix of [`UInt128`](Self::UInt128) elements
    UI128Matrix,
    /// A matrix of [`Float32`](Self::Float32) elements
    F32Matrix,
    #[strum(serialize = "F64Matrix", serialize = "Matrix")]
    /// A matrix of [`Float64`](Self::Float64) elements
    F64Matrix,
    /// A matrix of [`Float128`](Self::Float128) elements
    F128Matrix,
    /// A matrix of [`Complex32`](Self::Complex32) elements
    C32Matrix,
    /// A matrix of [`Complex64`](Self::Complex64) elements
    C64Matrix,
    /// A matrix of [`Complex128`](Self::Complex128) elements
    C128Matrix,
}
impl PropertyType {
    /// Returns true iff this type is a scalar
    ///
    /// Does not return true for vectors or matrices with scalar elements
    pub fn is_scalar(&self) -> bool {
        const FIRST: u8 = PropertyType::Boolean as u8;
        const LAST: u8 = PropertyType::Float128 as u8;
        (FIRST..=LAST).contains(&(*self as u8))
    }
    /// Returns true iff this type is complex
    ///
    /// Does not return true for vectors or matrices with complex elements
    pub fn is_complex(&self) -> bool {
        const FIRST: u8 = PropertyType::Complex32 as u8;
        const LAST: u8 = PropertyType::Complex128 as u8;
        (FIRST..=LAST).contains(&(*self as u8))
    }
    /// Returns true iff this type is a vector
    pub fn is_vector(&self) -> bool {
        const FIRST: u8 = PropertyType::I8Vector as u8;
        const LAST: u8 = PropertyType::C128Vector as u8;
        (FIRST..=LAST).contains(&(*self as u8))
    }
    /// Returns true iff this type is a matrix
    pub fn is_matrix(&self) -> bool {
        const FIRST: u8 = PropertyType::I8Matrix as u8;
        const LAST: u8 = PropertyType::C128Matrix as u8;
        (FIRST..=LAST).contains(&(*self as u8))
    }
    pub(crate) fn requires_data_block(&self) -> bool {
        self.is_vector() || self.is_matrix()
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

        let r#type = attrs.remove("type")
            .ok_or(context(MissingAttr))
            .attach_printable("Missing type attribute")?
            .parse::<PropertyType>()
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

        if r#type == PropertyType::String {
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
        } else if r#type.requires_data_block() {
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
/// For simple (atomic) types such as [Int32](PropertyType::Int32) or [TimePoint](PropertyType::TimePoint),
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
                if prop.r#type == PropertyType::$variant {
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
                if prop.r#type == PropertyType::$variant {
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
