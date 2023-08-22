use error_stack::{Report, report, ResultExt, IntoReport};
use libxml::readonly::RoNode;
use num_complex::Complex;
use time::{OffsetDateTime, format_description::well_known::Iso8601};

use crate::error::{ParseNodeError, ParseValueError};

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct FitsKeyword {
    pub name: String,
    pub value: String,
    pub comment: String,
}
impl FitsKeyword {
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, Report<ParseNodeError>> {
        let _span_guard = tracing::debug_span!("<FITSKeyword>").entered();
        const CONTEXT: ParseNodeError = ParseNodeError("FITSKeyword");
        let mut attrs = node.get_attributes();

        let name = attrs.remove("name")
            .ok_or(report!(CONTEXT))
            .attach_printable("Missing name attribute")?
            .trim()
            .to_string();

        let value = attrs.remove("value")
            .ok_or(report!(CONTEXT))
            .attach_printable("Missing value attribute")?
            .trim()
            .to_string();

        let comment = attrs.remove("comment")
            .ok_or(report!(CONTEXT))
            .attach_printable("Missing comment attribute")?
            .trim()
            .to_string();

        for remaining in attrs.into_iter() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }

        for child in node.get_child_nodes() {
            tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
        }

        Ok(Self {
            name,
            value,
            comment,
        })
    }
}

#[derive(Clone, Debug)]
pub struct FitsKeyValue {
    pub value: String,
    pub comment: String,
}

pub trait FromFitsStr: Sized {
    fn from_fits_str(val: &str) -> Result<Self, Report<ParseValueError>>;
}
impl FromFitsStr for String {
    fn from_fits_str(mut val: &str) -> Result<Self, Report<ParseValueError>> {
        const CONTEXT: ParseValueError = ParseValueError("FITS key as String");
        if val.starts_with('\'') && val.ends_with('\'') {
            val = &val[1..val.len()-1];
            let no_trailing_spaces = val.trim_end_matches(' ');
            if no_trailing_spaces.len() == 0 && val.len() > 0 {
                // EMPTY STRING: the first space is considered semantically important, but '      ' and ' ' are considered equal
                Ok(" ".to_string())
            } else {
                // note that this is also the fallthrough condition for NULL STRINGS
                // that is, '' will be correctly parsed as "" instead of " "

                // single quotes used inside of the string are escaped by doubling up
                Ok(no_trailing_spaces.replace("\'\'", "\'"))
            }
        } else {
            Err(report!(CONTEXT)).attach_printable("Must start and end with a single quote character \'")
        }
    }
}
impl FromFitsStr for bool {
    fn from_fits_str(val: &str) -> Result<Self, Report<ParseValueError>> {
        const CONTEXT: ParseValueError = ParseValueError("FITS key as bool");
        match val {
            "T" => Ok(true),
            "F" => Ok(false),
            bad => Err(report!(CONTEXT)).attach_printable(format!("Expected one of [T, F], found {bad}"))
        }
    }
}
macro_rules! from_fits_str_real {
    ($t:ty) => {
        impl FromFitsStr for $t {
            fn from_fits_str(val: &str) -> Result<Self, Report<ParseValueError>> {
                val.parse().into_report().change_context(ParseValueError(concat!("FITS key as ", stringify!($t))))
            }
        }
    };
}
from_fits_str_real!(u8);
from_fits_str_real!(i8);
from_fits_str_real!(u16);
from_fits_str_real!(i16);
from_fits_str_real!(u32);
from_fits_str_real!(i32);
from_fits_str_real!(u64);
from_fits_str_real!(i64);
from_fits_str_real!(u128);
from_fits_str_real!(i128);

// TODO: support for [D exponents](https://fits.gsfc.nasa.gov/standard40/fits_standard40aa-le.pdf#Hfootnote.4)
from_fits_str_real!(f32);
from_fits_str_real!(f64);

macro_rules! from_fits_str_complex {
    ($t:ty) => {
        impl FromFitsStr for Complex<$t> {
            fn from_fits_str(val: &str) -> Result<Self, Report<ParseValueError>> {
                const CONTEXT: ParseValueError = ParseValueError(concat!("FITS key as Complex<", stringify!($t), '>'));
                if val.starts_with('(') && val.ends_with(')') {
                    if let Some((re, im)) = val[1..val.len()-1].split_once(',') {
                        Ok(Complex::<$t>::new(
                            re.trim().parse().into_report().change_context(CONTEXT)?,
                            im.trim().parse().into_report().change_context(CONTEXT)?,
                        ))
                    } else {
                        Err(report!(CONTEXT)).attach_printable("Missing comma")
                    }
                } else {
                    Err(report!(CONTEXT)).attach_printable("Missing parentheses")
                }
            }
        }
    };
}
from_fits_str_complex!(u8);
from_fits_str_complex!(i8);
from_fits_str_complex!(u16);
from_fits_str_complex!(i16);
from_fits_str_complex!(u32);
from_fits_str_complex!(i32);
from_fits_str_complex!(u64);
from_fits_str_complex!(i64);
from_fits_str_complex!(u128);
from_fits_str_complex!(i128);

// TODO: support for [D exponents](https://fits.gsfc.nasa.gov/standard40/fits_standard40aa-le.pdf#Hfootnote.4)
from_fits_str_complex!(f32);
from_fits_str_complex!(f64);

impl FromFitsStr for OffsetDateTime {
    fn from_fits_str(val: &str) -> Result<Self, Report<ParseValueError>> {
        // TODO: large dates
        // TODO: make more rigorous (see [section 9.1.1](https://fits.gsfc.nasa.gov/standard40/fits_standard40aa-le.pdf#subsubsection.9.1.1) of the spec)
        OffsetDateTime::parse(val, &Iso8601::PARSING)
            .into_report()
            .change_context(ParseValueError("FITS key as OffsetDateTime"))
    }
}