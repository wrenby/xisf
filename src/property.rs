use error_stack::{report, Result, ResultExt, Report};
use libxml::{readonly::RoNode, tree::NodeType};
use num_complex::Complex;
use strum::{EnumString, Display};
use time::{OffsetDateTime, format_description::well_known::Iso8601};

use crate::{
    data_block::DataBlock,
    error::{ParseValueError, ParseNodeError, ParseNodeErrorKind::{self, *}},
    reference::is_valid_id, Context
};

#[repr(u8)]
#[derive(Clone, Copy, Debug, Display, EnumString, PartialEq)]
pub enum PropertyType {
    Boolean,
    Int8,
    #[strum(serialize = "UInt8", serialize = "Byte")]
    UInt8,
    #[strum(serialize = "Int16", serialize = "Short")]
    Int16,
    #[strum(serialize = "UInt16", serialize = "UShort")]
    UInt16,
    #[strum(serialize = "Int32", serialize = "Int")]
    Int32,
    #[strum(serialize = "UInt32", serialize = "UInt")]
    UInt32,
    Int64,
    UInt64,
    Int128,
    UInt128,
    #[strum(serialize = "Float32", serialize = "Float")]
    Float32,
    #[strum(serialize = "Float64", serialize = "Double")]
    Float64,
    #[strum(serialize = "Float128", serialize = "Quad")]
    Float128,
    Complex32,
    #[strum(serialize = "Complex64", serialize = "Complex")]
    Complex64,
    Complex128,
    String,
    TimePoint,
    I8Vector,
    #[strum(serialize = "UI8Vector", serialize = "ByteArray")]
    UI8Vector,
    I16Vector,
    UI16Vector,
    #[strum(serialize = "I32Vector", serialize = "IVector")]
    I32Vector,
    #[strum(serialize = "UI32Vector", serialize = "UIVector")]
    UI32Vector,
    I64Vector,
    UI64Vector,
    I128Vector,
    UI128Vector,
    F32Vector,
    #[strum(serialize = "F64Vector", serialize = "Vector")]
    F64Vector,
    F128Vector,
    C32Vector,
    C64Vector,
    C128Vector,
    I8Matrix,
    #[strum(serialize = "UI8Matrix", serialize = "ByteMatrix")]
    UI8Matrix,
    I16Matrix,
    UI16Matrix,
    #[strum(serialize = "I32Matrix", serialize = "IMatrix")]
    I32Matrix,
    #[strum(serialize = "UI32Matrix", serialize = "UIMatrix")]
    UI32Matrix,
    I64Matrix,
    UI64Matrix,
    I128Matrix,
    UI128Matrix,
    F32Matrix,
    #[strum(serialize = "F64Matrix", serialize = "Matrix")]
    F64Matrix,
    F128Matrix,
    C32Matrix,
    C64Matrix,
    C128Matrix,
}
impl PropertyType {
    pub fn is_scalar(&self) -> bool {
        const FIRST: u8 = PropertyType::Boolean as u8;
        const LAST: u8 = PropertyType::Float128 as u8;
        (FIRST..=LAST).contains(&(*self as u8))
    }
    /// Does not return true for complex vectors or complex matrices
    pub fn is_complex(&self) -> bool {
        const FIRST: u8 = PropertyType::Complex32 as u8;
        const LAST: u8 = PropertyType::Complex128 as u8;
        (FIRST..=LAST).contains(&(*self as u8))
    }
    pub fn is_vector(&self) -> bool {
        const FIRST: u8 = PropertyType::I8Vector as u8;
        const LAST: u8 = PropertyType::C128Vector as u8;
        (FIRST..=LAST).contains(&(*self as u8))
    }
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

#[derive(Clone, Debug, PartialEq)]
pub enum PropertyValue {
    Plaintext(String),
    DataBlock(DataBlock),
}

#[derive(Clone, Debug)]
pub struct PropertyContent {
    pub r#type: PropertyType,
    pub value: PropertyValue,
    pub comment: Option<String>,
}

pub trait FromProperty: Sized {
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
