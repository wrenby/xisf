use std::collections::HashMap;

use error_stack::{Report, report, Result, ResultExt};
use libxml::readonly::RoNode;

use crate::error::{ParseNodeError, ParseNodeErrorKind::{self, *}};

fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}
const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("RGBWorkingSpace", kind)
}

/// Describes the location of the red, green, and blue primary colors in the CIE 1931 chromaticity diagram using the
/// [xyY coordinate system](https://en.wikipedia.org/wiki/CIE_1931_color_space#CIE_xy_chromaticity_diagram_and_the_CIE_xyY_color_space)
///
/// <div class="warning">XISF color spaces are specified relative to D50 white, commonly used for print media,
/// not the D65 white more commonly used for digital media.</div>
#[derive(Clone, Debug)]
pub struct RGBWorkingSpace {
    /// A common name for this RGB working space
    pub name: Option<String>,
    /// A transformation from RGB coordinate space into linear CIE XYZ color space
    pub linearization: Linearization,
    /// The x coordinates of primary colors in CIE xyY color space, arranged in R,G,B order
    pub chromaticity_x: [f32; 3],
    /// The y coordinates of primary colors in CIE xyY color space, arranged in R,G,B order
    pub chromaticity_y: [f32; 3],
    /// The Y coordinates of primary colors in CIE xyY color space, arranged in R,G,B order
    pub luminance: [f32; 3],
}
impl Default for RGBWorkingSpace {
    fn default() -> Self {
        Self {
            name: Some("sRGB (D50)".to_string()),
            linearization: Linearization::Srgb,
            chromaticity_x: [0.648431, 0.321152, 0.155886],
            chromaticity_y: [0.330856, 0.597871, 0.066044],
            luminance: [0.222491, 0.716888, 0.060621],
        }
    }
}
impl RGBWorkingSpace {
    // TODO: move this type of function to ImageData or something; doesn't make any sense to calculate M for each pixel sample
    // pub fn rgb_to_cie_xyz(&self, rgb: &[f32; 3]) -> [f32; 3] {
    //     let nominal = rgb.map(|x| self.transfer_function.apply(x));
    //     let x = &self.chromaticity_x;
    //     let y = &self.chromaticity_y;
    //     let lum = &self.luminance;

    //     let m00 = lum[0] * x[0] / y[0];
    //     let m01 = lum[1] * x[1] / y[1];
    //     let m02 = lum[2] * x[2] / y[2];

    //     let m20 = lum[0] * (1.0 - x[0] - y[0]) / y[0];
    //     let m21 = lum[1] * (1.0 - x[1] - y[1]) / y[1];
    //     let m22 = lum[2] * (1.0 - x[2] - y[2]) / y[2];

    //     let m = ndarray::arr2(&[
    //         [m00, m01, m02],
    //         self.luminance,
    //         [m20, m21, m22]
    //     ]);
    //     m.dot(&ArrayView1::from(&nominal))
    //         // this unwrap is safe because we know the result is contiguous and in memory order
    //         .as_slice().unwrap()
    //         // this unwrap is safe because we know the size of the product of a 3x3 and a 3x1 matrix is 3x1
    //         .try_into().unwrap()
    // }
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, ParseNodeError> {
        let _span_guard = tracing::debug_span!("RGBWorkingSpace");

        let mut attrs = node.get_attributes();
        let children = node.get_child_nodes();

        let name = attrs.remove("name");

        let linearization = if let Some(gamma) = attrs.remove("gamma") {
            match gamma.as_str() {
                "sRGB" => Linearization::Srgb,
                other => {
                    let number = other
                        .trim()
                        .parse::<f32>()
                        .change_context(context(InvalidAttr))
                        .attach_printable_lazy(|| format!("Invalid gamma attribute: expected either \"sRGB\" or a floating-point value greater than zero, found {other}"))?;
                    if number > 0.0 {
                        Linearization::Gamma(number)
                    } else {
                        return Err(report(InvalidAttr)).attach_printable(format!("Invalid gamma attribute: must be greater than zero, found {other}"))
                    }
                },
            }
        } else {
            return Err(report(MissingAttr)).attach_printable("Missing gamma attribute")
        };

        fn parse_coordinates(attr: &str, attrs: &mut HashMap<String, String>) -> Result<[f32; 3], ParseNodeError> {
            if let Some(val) = attrs.remove(attr) {
                match val.split(":").collect::<Vec<_>>().as_slice() {
                    &[r, g, b] => Ok([
                        r.trim().parse::<f32>()
                            .change_context(context(InvalidAttr))
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse red coordinate"))?,
                        g.trim().parse::<f32>()
                            .change_context(context(InvalidAttr))
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse green coordinate"))?,
                        b.trim().parse::<f32>()
                            .change_context(context(InvalidAttr))
                            .attach_printable_lazy(|| format!("Invalid {attr} attribute: failed to parse blue coordinate"))?,
                    ]),
                    _bad => Err(report(InvalidAttr)).attach_printable(format!("Invalid {attr} attribute: expected pattern r:g:b, found {val}")),
                }
            } else {
                Err(report(MissingAttr)).attach_printable(format!("Missing {attr} attribute"))
            }
        }

        let chromaticity_x = parse_coordinates("x", &mut attrs)?;
        let chromaticity_y = parse_coordinates("y", &mut attrs)?;
        let luminance = parse_coordinates("Y", &mut attrs)?;

        for remaining in attrs.into_iter() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }
        for child in children {
            tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
        }

        Ok(Self {
            name,
            linearization,
            chromaticity_x,
            chromaticity_y,
            luminance,
        })
    }
}

/// A transformation from nonlinear color space into linear color space
#[derive(Clone, Debug, PartialEq)]
pub enum Linearization {
    /// For a non-sRGB color space, raises the sample to the power of gamma
    Gamma(f32),
    /// For an sRGB color space, applies a specific piecewise function
    Srgb,
}
impl Linearization {
    // pub(crate) fn apply(&self, x: f32) -> f32 {
    //     match self {
    //         &Self::Gamma(gamma) => x.powf(gamma),
    //         Self::Srgb => {
    //             if x <= 0.04045 {
    //                 x / 12.92
    //             } else {
    //                 ((x + 0.055) / 1.055).powf(2.4)
    //             }
    //         }
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use libxml::parser::Parser;

    #[test]
    fn parse_node() {
        // examples from the XISF spec
        let adobe_rgb = r#"<RGBWorkingSpace x="0.648431:0.230154:0.155886"
            y="0.330856:0.701572:0.066044"
            Y="0.311114:0.625662:0.063224" gamma="2.2"
            name="Adobe RGB (1998)" />"#;

        let xml = Parser::default().parse_string(adobe_rgb.as_bytes()).unwrap();
        let rgbws = RGBWorkingSpace::parse_node(xml.get_root_readonly().unwrap());
        assert!(rgbws.is_ok());
        let rgbws = rgbws.unwrap();
        assert_eq!(rgbws.chromaticity_x, [0.648431, 0.230154, 0.155886]);
        assert_eq!(rgbws.chromaticity_y, [0.330856, 0.701572, 0.066044]);
        assert_eq!(rgbws.luminance, [0.311114, 0.625662, 0.063224]);
        assert_eq!(rgbws.linearization, Linearization::Gamma(2.2));
        assert_eq!(rgbws.name.as_deref(), Some("Adobe RGB (1998)"));

        let srgb = r#"<RGBWorkingSpace x="0.648431:0.321152:0.155886"
            y="0.330856:0.597871:0.066044"
            Y="0.222491:0.716888:0.060621" gamma="sRGB"
            name="sRGB IEC61966-2.1" />"#;

        let xml = Parser::default().parse_string(srgb.as_bytes()).unwrap();
        let rgbws = RGBWorkingSpace::parse_node(xml.get_root_readonly().unwrap());
        assert!(rgbws.is_ok());
        let rgbws = rgbws.unwrap();
        assert_eq!(rgbws.chromaticity_x, [0.648431, 0.321152, 0.155886]);
        assert_eq!(rgbws.chromaticity_y, [0.330856, 0.597871, 0.066044]);
        assert_eq!(rgbws.luminance, [0.222491, 0.716888, 0.060621]);
        assert_eq!(rgbws.linearization, Linearization::Srgb);
        assert_eq!(rgbws.name.as_deref(), Some("sRGB IEC61966-2.1"));

        let srgb_uniform_linear = r#"<RGBWorkingSpace x="0.648431:0.230154:0.155886"
            y="0.330856:0.701572:0.066044"
            Y="0.333333:0.333333:0.333333" gamma="1"
            name="Uniform Linear with sRGB Primaries" />"#;

        let xml = Parser::default().parse_string(srgb_uniform_linear.as_bytes()).unwrap();
        let rgbws = RGBWorkingSpace::parse_node(xml.get_root_readonly().unwrap());
        assert!(rgbws.is_ok());
        let rgbws = rgbws.unwrap();
        assert_eq!(rgbws.chromaticity_x, [0.648431, 0.230154, 0.155886]);
        assert_eq!(rgbws.chromaticity_y, [0.330856, 0.701572, 0.066044]);
        assert_eq!(rgbws.luminance, [0.333333, 0.333333, 0.333333]);
        assert_eq!(rgbws.linearization, Linearization::Gamma(1.0));
        assert_eq!(rgbws.name.as_deref(), Some("Uniform Linear with sRGB Primaries"));
    }
}
