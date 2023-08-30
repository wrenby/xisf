use std::collections::HashMap;

use error_stack::{Report, report, ResultExt};
use libxml::readonly::RoNode;

use crate::error::ParseNodeError;

/// A single-channel screen transfer function
#[derive(Clone, Debug, PartialEq)]
pub struct STF {
    midtones_balance: f64,
    shadows_clip: f64,
    highlights_clip: f64,
    shadows_expansion: f64,
    highlights_expansion: f64,

    clip_delta: f64,
    expand_delta: f64,
}
/// [Identity transformation](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__equation_23__)
impl Default for STF {
    fn default() -> Self {
        Self {
            midtones_balance: 0.5,
            shadows_clip: 0.0,
            highlights_clip: 1.0,
            shadows_expansion: 0.0,
            highlights_expansion: 1.0,
            clip_delta: 1.0,
            expand_delta: 1.0,
        }
    }
}
impl STF {
    /// Parameters:
    /// - `m`: midtones balance, clamped to the range [0, 1]
    /// - `s`: shadows clipping point, clamped to the range [0, 1]
    /// - `h`: highlights clipping point, clamped to the range [0, 1]
    /// - `l`: shadows dynamic range expansion, clamped to 0 or below
    /// - `r`: highlights dynamic range expansion, clamped to 1 or above
    ///
    /// If `s` is greater than `h`, the two are swapped
    pub fn new(m: f64, mut s: f64, mut h: f64, l: f64, r: f64) -> Self {
        if s > h {
            std::mem::swap(&mut s, &mut h);
        }
        let shadows_clip = s.clamp(0.0, 1.0);
        let highlights_clip = h.clamp(0.0, 1.0);
        let shadows_expansion = l.min(0.0);
        let highlights_expansion = r.max(1.0);
        Self {
            midtones_balance: m.clamp(0.0, 1.0),
            shadows_clip,
            highlights_clip,
            shadows_expansion,
            highlights_expansion,
            clip_delta: highlights_clip - shadows_clip,
            expand_delta: highlights_expansion - shadows_expansion,
        }
    }

    pub fn mb(&self) -> f64 {
        self.midtones_balance
    }

    pub fn sc(&self) -> f64 {
        self.shadows_clip
    }

    pub fn hc(&self) -> f64 {
        self.highlights_clip
    }

    pub fn se(&self) -> f64 {
        self.shadows_expansion
    }

    pub fn he(&self) -> f64 {
        self.highlights_expansion
    }

    // pub fn make_8bit_lut(&self)
    // pub fn make_16bit_lut(&self)
    // pub fn make_20bit_lut(&self)
    // pub fn make_24bit_lut(&self)

    /// [Midtones transfer function](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__equation_19__)
    #[inline]
    fn mtf(&self, x: f64) -> f64 {
        if x > 0.0 {
            if x < 1.0 {
                let m1 = self.mb() - 1.0;
                m1 * x / ((self.mb() + m1) * x - self.mb())
            } else {
                1.0
            }
        } else {
            0.0
        }
    }

    /// [Clipping function](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__equation_20__)
    #[inline]
    fn clip(&self, x: f64) -> f64 {
        if x < self.sc() {
            0.0
        } else if x > self.hc() {
            1.0
        } else if self.clip_delta == 0.0 {
            self.sc()
        } else {
            (x - self.sc()) / self.clip_delta
        }
    }

    /// [Expansion function](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__equation_21__)
    #[inline]
    fn expand(&self, x: f64) -> f64 {
        (x - self.se()) / self.expand_delta
    }

    /// [Display function](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__equation_22__)
    #[inline]
    pub fn apply(&self, x: f64) -> f64 {
        self.expand(self.mtf(self.clip(x)))
    }
}

/// A multi-channel screen transfer function
#[derive(Clone, Debug)]
pub struct DisplayFunction {
    name: Option<String>,
    rgbl: [STF; 4],
}
/// No name and identity STFs for each channel
impl Default for DisplayFunction {
    fn default() -> Self {
        Self {
            name: Default::default(),
            rgbl: Default::default(),
        }
    }
}
/// Ignores the name, just compares the single-channel STFs
impl PartialEq for DisplayFunction {
    fn eq(&self, other: &Self) -> bool {
        self.rgbl == other.rgbl
    }
    fn ne(&self, other: &Self) -> bool {
        self.rgbl != other.rgbl
    }
}
impl DisplayFunction {
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, Report<ParseNodeError>> {
        const CONTEXT: ParseNodeError = ParseNodeError("DisplayFunction");
        let _span_guard = tracing::debug_span!("DisplayFunction");
        let mut attrs = node.get_attributes();
        let children = node.get_child_nodes();

        let name = attrs.remove("name");

        fn parse_rkgbl(attr: &str, attrs: &mut HashMap<String, String>) -> Result<[f64; 4], Report<ParseNodeError>> {
            if let Some(val) = attrs.remove(attr) {
                match val.split(":").collect::<Vec<_>>().as_slice() {
                    &[rk, g, b, l] => Ok([
                        rk.trim().parse::<f64>()
                            .change_context(CONTEXT)
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse red/grayscale value"))?,
                        g.trim().parse::<f64>()
                            .change_context(CONTEXT)
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse green value"))?,
                        b.trim().parse::<f64>()
                            .change_context(CONTEXT)
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse blue value"))?,
                        l.trim().parse::<f64>()
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

        let rk = STF::new(m[0], s[0], h[0], l[0], r[0]);
        let g  = STF::new(m[1], s[1], h[1], l[1], r[1]);
        let b  = STF::new(m[2], s[2], h[2], l[2], r[2]);
        let l  = STF::new(m[3], s[3], h[3], l[3], r[3]);

        for child in children {
            tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
        }

        Ok(Self {
            name,
            rgbl: [rk, g, b, l],
        })
    }

    /// A common name used to distinguish this display function from others or describe its behavior.
    /// May contain any valid Unicode.
    #[inline]
    pub fn name(&self) -> &Option<String> {
        &self.name
    }

    /// Screen transfer functions for the red channel of an RGB image
    #[inline]
    pub fn r(&self) -> &STF {
        &self.rgbl[0]
    }
    /// Screen transfer function for the single channel of a grayscale image -- just an alias for [`Self::r()`]
    #[inline]
    pub fn k(&self) -> &STF {
        &self.rgbl[0]
    }
    /// Screen transfer functions for the red channel of an RGB image
    #[inline]
    pub fn g(&self) -> &STF {
        &self.rgbl[1]
    }
    /// Screen transfer functions for the red channel of an RGB image
    #[inline]
    pub fn b(&self) -> &STF {
        &self.rgbl[2]
    }
    /// Screen transfer function for grayscale/luminance previews of an RGB image
    ///
    /// <div class="warning">Do not use this for grayscale images! This STF is only meant to be used
    /// for grayscale previews of color images. For grayscale images, [`Self::k()`] should be used instead.</div>
    #[inline]
    pub fn l(&self) -> &STF {
        &self.rgbl[3]
    }
}

/// Allows transparent access to the per-channel [`STF`]
impl std::ops::Index<usize> for DisplayFunction {
    type Output = STF;

    fn index(&self, index: usize) -> &Self::Output {
        &self.rgbl[index]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use libxml::parser::Parser;

    #[test]
    fn parse_node() {
        // example from the XISF spec
        let auto_stretch = r#"<DisplayFunction m="0.000735:0.000735:0.000735:0.5"
            s="0.003758:0.003758:0.003758:0"
            h="1:1:1:1"
            l="0:0:0:0"
            r="1:1:1:1"
            name="AutoStretch" />"#;

        let xml = Parser::default().parse_string(auto_stretch.as_bytes()).unwrap();
        let df = DisplayFunction::parse_node(xml.get_root_readonly().unwrap());
        let df = df.unwrap();
        assert_eq!(df.name.as_deref(), Some("AutoStretch"));
        let rgb = STF::new(0.000735, 0.003758, 1.0, 0.0, 1.0);
        assert_eq!(df.r(), &rgb);
        assert_eq!(df.g(), &rgb);
        assert_eq!(df.b(), &rgb);
        assert_eq!(df.l(), &Default::default());

        // should clamp midpoint balance to 1.0, swap high clip and low clip,
        // clamp shadows_expansion to 0, and clamp highlights_expansion to 1
        let malformed = r#"<DisplayFunction m="1.000735:1.000735:1.000735:0.5"
            s="1:1:1:0"
            h="0.003758:0.003758:0.003758:1"
            l="1:1:1:0"
            r="0:0:0:1" />"#;
        let xml = Parser::default().parse_string(malformed.as_bytes()).unwrap();
        let df = DisplayFunction::parse_node(xml.get_root_readonly().unwrap());
        let df = df.unwrap();
        assert_eq!(df.name, None);
        let rgb = STF::new(1.0, 0.003758, 1.0, 0.0, 1.0);
        assert_eq!(df.r(), &rgb);
        assert_eq!(df.g(), &rgb);
        assert_eq!(df.b(), &rgb);
        assert_eq!(df.l(), &Default::default());
    }
}