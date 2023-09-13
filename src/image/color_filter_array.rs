use error_stack::{Report, report, Result, ResultExt};
use libxml::readonly::RoNode;
use ndarray::{Array2, Axis};
use parse_int::parse as parse_auto_radix;

use crate::error::{ParseNodeError, ParseNodeErrorKind::{self, *}};

fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}
const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("ColorFilterArray", kind)
}

/// A [color filter array](https://en.wikipedia.org/wiki/Color_filter_array), a repeating pattern of microscopic filters in front of of a sensor's pixels
///
/// Used to assign color data to raw images. Supported filters: undefined, red, green, blue, white/panchromatic, cyan, magenta, yellow
#[derive(Clone, Debug)]
pub struct CFA {
    name: Option<String>,
    matrix: Array2<char>,
}
impl CFA {
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, ParseNodeError> {
        let _span_guard = tracing::debug_span!("ColorFilterArray");
        let mut attrs = node.get_attributes();
        let children = node.get_child_nodes();

        let name = attrs.remove("name");

        // undefined, red, green, blue, white/panchromatic, cyan, magenta, yellow
        let valid_chars = ['0', 'R', 'G', 'B', 'W', 'C', 'M', 'Y'];

        let pattern = attrs.remove("pattern")
            .ok_or(report(MissingAttr))
            .attach_printable("Missing pattern attribute")?
            .chars()
            .map(|c| {
                if valid_chars.contains(&c) {
                    Ok(c)
                } else {
                    Err(report(InvalidAttr))
                        .attach_printable(format!("Invalid CFA element in pattern attribute: Expected one of {valid_chars:?}, found {c}"))
                }
            })
            // .collect()ing a iterator of results into a result of a container functions as a way to verify all the characters have been parsed correctly
            // If any of them failed to parse, then the result of the collect() will be the first Err
            .collect::<Result<Vec<_>, ParseNodeError>>()?;

        let height = attrs.remove("height")
            .ok_or(report(MissingAttr))
            .attach_printable("Missing height attribute")?;
        let height = parse_auto_radix::<usize>(height.trim())
            .change_context(context(InvalidAttr))
            .attach_printable("Invalid height attribute: failed to parse as usize")?;

        let width = attrs.remove("width")
            .ok_or(report(MissingAttr))
            .attach_printable("Missing width attribute")?;
        let width = parse_auto_radix::<usize>(width.trim())
            .change_context(context(InvalidAttr))
            .attach_printable("Invalid width attribute: failed to parse as usize")?;

        let len = pattern.len();
        let matrix = Array2::from_shape_vec((height, width), pattern)
            .change_context(context(InvalidAttr))
            .attach_printable_lazy(|| format!("Expected H={height} * W={width} => {} elements in CFA pattern; found {}", height*width, len))?;

        for remaining in attrs.into_iter() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }
        for child in children {
            tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
        }

        Ok(Self {
            name,
            matrix,
        })
    }

    /// A common name for this pattern, e.g. "GRBG Bayer Filter" or "Fujifilm X-Trans Filter"
    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    /// The width of this pattern
    ///
    /// Recall that XISF images are stored in row-major order,
    /// so this should be tiled along dimension 1 (zero-indexed)
    pub fn width(&self) -> usize {
        self.matrix.len_of(Axis(1))
    }

    /// The height of this pattern
    ///
    /// Recall that XISF images are stored in row-major order,
    /// so this should be tiled along dimension 0 (zero-indexed)
    pub fn height(&self) -> usize {
        self.matrix.len_of(Axis(0))
    }
}
impl ToString for CFA {
    fn to_string(&self) -> String {
        self.matrix.iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use libxml::parser::Parser;

    #[test]
    fn parse_node() {
        // examples from the XISF spec
        let bayer = r#"<ColorFilterArray pattern="GRBG" width="2" height="2" name="GRBG Bayer Filter" />"#;

        let xml = Parser::default().parse_string(bayer.as_bytes()).expect("parse XML");
        let cfa = CFA::parse_node(xml.get_root_readonly().expect("get root")).expect("parse CFA");
        assert_eq!(cfa.name(), Some("GRBG Bayer Filter"));
        assert_eq!(cfa.height(), 2);
        assert_eq!(cfa.width(), 2);
        assert_eq!(cfa.to_string().as_str(), "GRBG");


        let x_trans = r#"<ColorFilterArray pattern="GBGGRGRGRBGBGBGGRGGRGGBGBGBRGRGRGGBG"
            width="6" height="6" name="Fujifilm X-Trans Filter" />"#;

        let xml = Parser::default().parse_string(x_trans.as_bytes()).expect("parse XML");
        let cfa = CFA::parse_node(xml.get_root_readonly().expect("get root")).expect("parse CFA");
        assert_eq!(cfa.name(), Some("Fujifilm X-Trans Filter"));
        assert_eq!(cfa.height(), 6);
        assert_eq!(cfa.width(), 6);
        assert_eq!(cfa.to_string().as_str(), "GBGGRGRGRBGBGBGGRGGRGGBGBGBRGRGRGGBG");


        let true_sense = r#"<ColorFilterArray pattern="GWRWWGWRBWGWWBWG"
        width="4" height="4" name="Kodak TRUESENSE Sparse Color Filter" />"#;

        let xml = Parser::default().parse_string(true_sense.as_bytes()).expect("parse XML");
        let cfa = CFA::parse_node(xml.get_root_readonly().expect("get root")).expect("parse CFA");
        assert_eq!(cfa.name(), Some("Kodak TRUESENSE Sparse Color Filter"));
        assert_eq!(cfa.height(), 4);
        assert_eq!(cfa.width(), 4);
        assert_eq!(cfa.to_string().as_str(), "GWRWWGWRBWGWWBWG");
    }
}
