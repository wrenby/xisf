use std::collections::HashMap;

use error_stack::{Report, report, ResultExt};
use libxml::readonly::RoNode;

use crate::error::ParseNodeError;

#[derive(Clone, Debug, PartialEq)]
pub struct HistogramTransformation {
    pub midtones_balance: f32,
    pub shadows_clip: f32,
    pub highlights_clip: f32,
    pub shadows_expansion: f32,
    pub highlights_expansion: f32,
}
/// Identity transformation
impl Default for HistogramTransformation {
    fn default() -> Self {
        Self {
            midtones_balance: 0.5,
            shadows_clip: 0.0,
            highlights_clip: 1.0,
            shadows_expansion: 0.0,
            highlights_expansion: 1.0,
        }
    }
}
impl HistogramTransformation {
    pub fn new(m: f32, s: f32, h: f32, l: f32, r: f32) -> Self {
        Self {
            midtones_balance: m,
            shadows_clip: s,
            highlights_clip: h,
            shadows_expansion: l,
            highlights_expansion: r,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct DisplayFunction {
    pub name: Option<String>,
    /// Histogram transform functions for red/gray, green, blue, and CIE L\*a\*b\* luminance, respectively
    pub rkgbl: [HistogramTransformation; 4],
}
/// == implementation ignores the name
impl PartialEq for DisplayFunction {
    fn eq(&self, other: &Self) -> bool {
        self.rkgbl == other.rkgbl
    }
    fn ne(&self, other: &Self) -> bool {
        self.rkgbl != other.rkgbl
    }
}
impl DisplayFunction {
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, Report<ParseNodeError>> {
        const CONTEXT: ParseNodeError = ParseNodeError("DisplayFunction");
        let _span_guard = tracing::debug_span!("DisplayFunction");
        let mut attrs = node.get_attributes();
        let children = node.get_child_nodes();

        let name = attrs.remove("name");

        fn parse_rkgbl(attr: &str, attrs: &mut HashMap<String, String>) -> Result<[f32; 4], Report<ParseNodeError>> {
            if let Some(val) = attrs.remove(attr) {
                match val.split(":").collect::<Vec<_>>().as_slice() {
                    &[rk, g, b, l] => Ok([
                        rk.trim().parse::<f32>()
                            .change_context(CONTEXT)
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse red/grayscale value"))?,
                        g.trim().parse::<f32>()
                            .change_context(CONTEXT)
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse green value"))?,
                        b.trim().parse::<f32>()
                            .change_context(CONTEXT)
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse blue value"))?,
                        l.trim().parse::<f32>()
                            .change_context(CONTEXT)
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse luminance value"))?,
                    ]),
                    _bad => Err(report!(CONTEXT)).attach_printable(format!("Invalid {attr} attribute: expected pattern rk:g:b:l, found {val}")),
                }
            } else {
                Err(report!(CONTEXT)).attach_printable(format!("Missing {attr} attribute"))
            }
        }

        let m = parse_rkgbl("m", &mut attrs)?;
        let s = parse_rkgbl("s", &mut attrs)?;
        let h = parse_rkgbl("h", &mut attrs)?;
        let l = parse_rkgbl("l", &mut attrs)?;
        let r = parse_rkgbl("r", &mut attrs)?;

        let rk = HistogramTransformation::new(m[0], s[0], h[0], l[0], r[0]);
        let g  = HistogramTransformation::new(m[1], s[1], h[1], l[1], r[1]);
        let b  = HistogramTransformation::new(m[2], s[2], h[2], l[2], r[2]);
        let l  = HistogramTransformation::new(m[3], s[3], h[3], l[3], r[3]);

        for child in children {
            tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
        }

        Ok(Self {
            name,
            rkgbl: [rk, g, b, l],
        })
    }

    pub fn rk(&self) -> &HistogramTransformation {
        &self.rkgbl[0]
    }
    pub fn g(&self) -> &HistogramTransformation {
        &self.rkgbl[1]
    }
    pub fn b(&self) -> &HistogramTransformation {
        &self.rkgbl[2]
    }
    pub fn l(&self) -> &HistogramTransformation {
        &self.rkgbl[3]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use libxml::parser::Parser;

    #[test]
    fn parse_node() {
        // examples from the XISF spec
        let auto_stretch = r#"<DisplayFunction m="0.000735:0.000735:0.000735:0.5"
            s="0.003758:0.003758:0.003758:0"
            h="1:1:1:1"
            l="0:0:0:0"
            r="1:1:1:1"
            name="AutoStretch" />"#;

        let xml = Parser::default().parse_string(auto_stretch.as_bytes()).unwrap();
        let df = DisplayFunction::parse_node(xml.get_root_readonly().unwrap());
        assert!(df.is_ok());
        let df = df.unwrap();
        assert_eq!(df.name.as_deref(), Some("AutoStretch"));
        let rkgb = HistogramTransformation::new(0.000735, 0.003758, 1.0, 0.0, 1.0);
        assert_eq!(df.rk(), &rkgb);
        assert_eq!(df.g(), &rkgb);
        assert_eq!(df.b(), &rkgb);
        assert_eq!(df.l(), &Default::default());
    }
}