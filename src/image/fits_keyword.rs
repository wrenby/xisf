use error_stack::{Report, report, Result, ResultExt};
use libxml::readonly::RoNode;
use num_complex::Complex;
use time::{OffsetDateTime, format_description::well_known::Iso8601};

use crate::error::{ParseNodeError, ParseValueError, ParseNodeErrorKind::{self, *}};

fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}
const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("FitsKeyword", kind)
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct FitsKeyword {
    pub name: String,
    pub content: FitsKeyContent,
}
impl FitsKeyword {
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, ParseNodeError> {
        let _span_guard = tracing::debug_span!("FITSKeyword").entered();
        let mut attrs = node.get_attributes();

        let name = attrs.remove("name")
            .ok_or(report(MissingAttr))
            .attach_printable("Missing name attribute")?
            .trim()
            .to_string();

        let value = attrs.remove("value")
            .ok_or(report(MissingAttr))
            .attach_printable("Missing value attribute")?
            .trim()
            .to_string();

        let comment = attrs.remove("comment")
            .ok_or(report(MissingAttr))
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
            content: FitsKeyContent {
                value,
                comment,
            },
        })
    }
}

/// POD type to store the value and comment of a FITS key
#[derive(Clone, Debug, PartialEq)]
pub struct FitsKeyContent {
    /// The value of the FITS key
    ///
    /// FITS keys are parsed lazily, so this will always be a string, regardless of type type it should be
    pub value: String,
    /// A comment explaining the use of or context for this key
    pub comment: String,
}
impl FitsKeyContent {
    #[cfg(test)]
    pub(crate) fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            comment: String::new(),
        }
    }
}

/// Convert a FITS key to a concrete type
pub trait FromFitsKey: Sized {
    /// Attempt to parse a FITS key as `Self`
    fn from_fits_key(content: &FitsKeyContent) -> Result<Self, ParseValueError>;
}
impl FromFitsKey for String {
    fn from_fits_key(content: &FitsKeyContent) -> Result<Self, ParseValueError> {
        const CONTEXT: ParseValueError = ParseValueError("FITS key as String");
        let mut val = content.value.trim();
        if val.starts_with('\'') && val.ends_with('\'') {
            val = &val[1..val.len()-1];
            let no_trailing_spaces = val.trim_end_matches(' ');
            if no_trailing_spaces.len() == 0 && val.len() > 0 {
                // EMPTY STRING: the first space is considered semantically important, but '      ' and ' ' are considered equal
                Ok(" ".to_string())
            } else {
                // note that this is also the fallthrough condition for NULL STRINGS
                // that is, '' will be correctly parsed as "" instead of " "

                if !val.is_ascii() {
                    tracing::warn!("FITS keys should be ASCII only!");
                }

                // single quotes used inside of the string are escaped by doubling up
                Ok(no_trailing_spaces.replace("\'\'", "\'"))
            }
        } else {
            Err(report!(CONTEXT)).attach_printable("Must start and end with a single quote character \'")
        }
    }
}
impl FromFitsKey for bool {
    fn from_fits_key(content: &FitsKeyContent) -> Result<Self, ParseValueError> {
        const CONTEXT: ParseValueError = ParseValueError("FITS key as bool");
        match content.value.trim() {
            "T" => Ok(true),
            "F" => Ok(false),
            bad => Err(report!(CONTEXT)).attach_printable(format!("Expected one of [T, F], found {bad}"))
        }
    }
}
macro_rules! from_fits_str_real {
    ($t:ty) => {
        impl FromFitsKey for $t {
            fn from_fits_key(content: &FitsKeyContent) -> Result<Self, ParseValueError> {
                content.value.trim().parse::<$t>().change_context(ParseValueError(concat!("FITS key as ", stringify!($t))))
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

macro_rules! from_fits_str_real_float {
    ($t:ty) => {
        impl FromFitsKey for $t {
            fn from_fits_key(content: &FitsKeyContent) -> Result<Self, ParseValueError> {
                content.value.trim()
                    .replace(|c| { c == 'd' || c == 'D' }, "E")
                    .parse::<$t>()
                    .change_context(ParseValueError(concat!("FITS key as ", stringify!($t))))
            }
        }
    };
}
from_fits_str_real_float!(f32);
from_fits_str_real_float!(f64);

macro_rules! from_fits_str_complex {
    ($t:ty) => {
        impl FromFitsKey for Complex<$t> {
            fn from_fits_key(content: &FitsKeyContent) -> Result<Self, ParseValueError> {
                const CONTEXT: ParseValueError = ParseValueError(concat!("FITS key as Complex<", stringify!($t), '>'));
                let val = content.value.trim();
                if val.starts_with('(') && val.ends_with(')') {
                    if let Some((re, im)) = val[1..val.len()-1].split_once(',') {
                        Ok(Complex::<$t>::new(
                            re.trim().parse::<$t>().change_context(CONTEXT)?,
                            im.trim().parse::<$t>().change_context(CONTEXT)?,
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

macro_rules! from_fits_str_complex_float {
    ($t:ty) => {
        impl FromFitsKey for Complex<$t> {
            fn from_fits_key(content: &FitsKeyContent) -> Result<Self, ParseValueError> {
                const CONTEXT: ParseValueError = ParseValueError(concat!("FITS key as Complex<", stringify!($t), '>'));
                let val = content.value.trim();
                if val.starts_with('(') && val.ends_with(')') {
                    if let Some((re, im)) = val[1..val.len()-1].split_once(',') {
                        Ok(Complex::<$t>::new(
                            re.trim()
                                .replace(|c| { c == 'd' || c == 'D' }, "E")
                                .parse::<$t>()
                                .change_context(CONTEXT)?,
                            im.trim()
                                .replace(|c| { c == 'd' || c == 'D' }, "E")
                                .parse::<$t>()
                                .change_context(CONTEXT)?,
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
from_fits_str_complex_float!(f32);
from_fits_str_complex_float!(f64);

impl FromFitsKey for OffsetDateTime {
    fn from_fits_key(content: &FitsKeyContent) -> Result<Self, ParseValueError> {
        // TODO: large dates
        // TODO: make more rigorous (see [section 9.1.1](https://fits.gsfc.nasa.gov/standard40/fits_standard40aa-le.pdf#subsubsection.9.1.1) of the spec)
        OffsetDateTime::parse(content.value.trim(), &Iso8601::PARSING)
            .change_context(ParseValueError("FITS key as OffsetDateTime"))
    }
}

// TODO: this is recursive
impl<T: FromFitsKey> FromFitsKey for (T, String) {
    fn from_fits_key(content: &FitsKeyContent) -> Result<Self, ParseValueError> {
        Ok((T::from_fits_key(content)?, content.comment.clone()))
    }
}

#[cfg(test)]
mod tests {
    use num_complex::Complex;

    use super::*;

    #[test]
    fn string_trims_trailing_whitespace() {
        let trailing = String::from_fits_key(&FitsKeyContent::new("'TABLE   '"));
        assert!(trailing.is_ok());
        assert_eq!(trailing.unwrap().as_str(), "TABLE", "trailing whitespace was not trimmed");

        let leading = String::from_fits_key(&FitsKeyContent::new("'  TABLE '"));
        assert!(leading.is_ok());
        assert_eq!(leading.unwrap().as_str(), "  TABLE", "leading whitespace was improperly trimmed");

        let around_single_quotes = String::from_fits_key(&FitsKeyContent::new("     'TABLE'   "));
        assert!(around_single_quotes.is_ok());
        assert_eq!(around_single_quotes.unwrap(), "TABLE", "whitespace around single quotes must be trimmed");
    }

    #[test]
    fn string_distinguishes_null_empty_undefined() {
        let null = String::from_fits_key(&FitsKeyContent::new("''"));
        assert!(null.is_ok());
        assert_eq!(null.unwrap().as_str(), "");

        let empty = String::from_fits_key(&FitsKeyContent::new("'        '"));
        assert!(empty.is_ok());
        assert_eq!(empty.unwrap().as_str(), " ", "the first space in an empty string is considered meaningful and should not be trimmed");

        let undefined = String::from_fits_key(&FitsKeyContent::new(""));
        assert!(undefined.is_err(), "parsing a key with a value of \"\" should be undefined");
    }

    #[test]
    fn string_unescapes_quotes() {
        let double_quotes = String::from_fits_key(&FitsKeyContent::new("'Now I''m falling asleep and she''s calling a cab'"));
        assert!(double_quotes.is_ok());
        assert_eq!(double_quotes.unwrap(), "Now I'm falling asleep and she's calling a cab");
    }

    macro_rules! test_real_base {
        ($t:ty, $v:literal) => {
            {
                let ideal = <$t>::from_fits_key(&FitsKeyContent::new(stringify!($v)));
                assert!(ideal.is_ok());
                assert_eq!(ideal.unwrap(), $v);

                let whitespace = <$t>::from_fits_key(&FitsKeyContent::new(concat!("     ", stringify!($v), "   ")));
                assert!(whitespace.is_ok());
                assert_eq!(whitespace.unwrap(), $v);

                let positive = <$t>::from_fits_key(&FitsKeyContent::new(concat!('+', stringify!($v))));
                assert!(positive.is_ok());
                assert_eq!(positive.unwrap(), $v);
            }
        }
    }
    macro_rules! test_real_neg {
        ($t:ty, $v:literal) => {
            {
                test_real_base!($t, $v);

                let negative = <$t>::from_fits_key(&FitsKeyContent::new(concat!('-', stringify!($v))));
                assert!(negative.is_ok());
                assert_eq!(negative.unwrap(), -$v);
            }
        }
    }
    macro_rules! test_real_float {
        ($t:ty, $v:literal) => {
            {
                test_real_neg!($t, $v);

                let e_exponent = <$t>::from_fits_key(&FitsKeyContent::new(concat!(stringify!($v), "E5")));
                assert!(e_exponent.is_ok());
                assert_eq!(e_exponent.unwrap(), $v * 100_000.0);

                // FITS floats sometimes encode exponents with the letter D instead of E
                // https://fits.gsfc.nasa.gov/standard40/fits_standard40aa-le.pdf#Hfootnote.4
                let d_exponent = <$t>::from_fits_key(&FitsKeyContent::new(concat!(stringify!($v), "D5")));
                assert!(d_exponent.is_ok());
                assert_eq!(d_exponent.unwrap(), $v * 100_000.0);
            }
        }
    }

    #[test]
    fn real() {
        test_real_base!(u8, 1);
        test_real_base!(u16, 1);
        test_real_base!(u32, 1);
        test_real_base!(u64, 1);
        test_real_base!(u128, 1);

        test_real_neg!(i8, 1);
        test_real_neg!(i16, 1);
        test_real_neg!(i32, 1);
        test_real_neg!(i64, 1);
        test_real_neg!(i128, 1);

        test_real_float!(f32, 1.0);
        test_real_float!(f64, 1.0);
    }

    macro_rules! test_complex {
        ($t:ty, $re:literal, $im:literal) => {
            {
                let val = Complex::<$t>::new($re, $im);

                let ideal = Complex::<$t>::from_fits_key(&FitsKeyContent::new(concat!('(', stringify!($re), ", ", stringify!($im), ')')));
                assert!(ideal.is_ok());
                assert_eq!(ideal.unwrap(), val);

                let no_parens = Complex::<$t>::from_fits_key(&FitsKeyContent::new(concat!(stringify!($re), ", ", stringify!($im))));
                assert!(no_parens.is_err());

                let no_comma = Complex::<$t>::from_fits_key(&FitsKeyContent::new(concat!('(', stringify!($re), ' ', stringify!($im), ')')));
                assert!(no_comma.is_err());

                let no_space_after_comma = Complex::<$t>::from_fits_key(&FitsKeyContent::new(concat!('(', stringify!($re), ',', stringify!($im), ')')));
                assert!(no_space_after_comma.is_ok());
                assert_eq!(no_space_after_comma.unwrap(), val);

                let whitespace_around_parens = Complex::<$t>::from_fits_key(&FitsKeyContent::new(concat!("   (", stringify!($re), ", ", stringify!($im), ")   ")));
                assert!(whitespace_around_parens.is_ok());
                assert_eq!(whitespace_around_parens.unwrap(), val);

                let whitespace_around_values = Complex::<$t>::from_fits_key(&FitsKeyContent::new(concat!("(   ", stringify!($re), "   ,   ", stringify!($im), "   )")));
                assert!(whitespace_around_values.is_ok());
                assert_eq!(whitespace_around_values.unwrap(), val);
            }
        }
    }

    #[test]
    fn complex() {
        test_complex!(u8, 1, 0);
        test_complex!(i8, 1, 0);
        test_complex!(u16, 1, 0);
        test_complex!(i16, 1, 0);
        test_complex!(u32, 1, 0);
        test_complex!(i32, 1, 0);
        test_complex!(u64, 1, 0);
        test_complex!(i64, 1, 0);
        test_complex!(u128, 1, 0);
        test_complex!(i128, 1, 0);
        test_complex!(f32, 1.0, 0.0);
        test_complex!(f64, 1.0, 0.0);
    }

    #[test]
    fn logical() {
        let t = bool::from_fits_key(&FitsKeyContent::new("T"));
        assert!(t.is_ok());
        assert_eq!(t.unwrap(), true);

        let f_whitespace = bool::from_fits_key(&FitsKeyContent::new("    F   "));
        assert!(f_whitespace.is_ok());
        assert_eq!(f_whitespace.unwrap(), false);
    }
}
