use error_stack::{Result, report, Report, ResultExt};
use libxml::readonly::RoNode;

use crate::error::{ParseNodeError, ParseNodeErrorKind::{self, *}};

fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}
const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("Resolution", kind)
}

/// The pixel density of the image, in pixels per inch or pixels per centimeter
///
/// Although it would be unusual for horizontal and vertical pixel density to differ,
/// as this would result in a stretched image for most media, they are specified separately.
#[derive(Clone, Debug)]
pub enum Resolution {
    /// Pixel density is measured in pixels per inch
    Inch {
        /// Horizontal pixel density, in pixels per inch
        ///
        /// Recall that XISF images are stored in row-major order,
        /// so this should be applied to dimension 1 (zero-indexed)
        horizontal: f32,
        /// Vertical pixel density, in pixels per inch
        ///
        /// Recall that XISF images are stored in row-major order,
        /// so this should be applied to dimension 0 (zero-indexed)
        vertical: f32,
    },
    /// Pixel density is measured in pixels per centimeter
    Cm {
        /// Horizontal pixel density, in pixels per centimeter
        ///
        /// Recall that XISF images are stored in row-major order,
        /// so this should be applied to dimension 1 (zero-indexed)
        horizontal: f32,
        /// Vertical pixel density, in pixels per centimeter
        ///
        /// Recall that XISF images are stored in row-major order,
        /// so this should be applied to dimension 0 (zero-indexed)
        vertical: f32,
    },
}
/// 72 PPI
impl Default for Resolution {
    fn default() -> Self {
        Self::Inch {
            horizontal: 72.0,
            vertical: 72.0,
        }
    }
}
impl Resolution {
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, ParseNodeError> {
        let _span_guard = tracing::debug_span!("Resolution");
        let mut attrs = node.get_attributes();
        let children = node.get_child_nodes();


        let horizontal = attrs.remove("horizontal")
            .ok_or(report(MissingAttr))?
            .trim()
            .parse::<f32>()
            .change_context(context(InvalidAttr))
            .attach_printable("Invalid horizontal attribute: failed to parse as f32")?;


        let vertical = attrs.remove("vertical")
            .ok_or(report(MissingAttr))?
            .trim()
            .parse::<f32>()
            .change_context(context(InvalidAttr))
            .attach_printable("Invalid horizontal attribute: failed to parse as f32")?;

        let unit = attrs.remove("unit");

        for remaining in attrs.into_iter() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }
        for child in children {
            tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
        }

        match unit.as_deref() {
            Some("inch") => Ok(Resolution::Inch { horizontal, vertical }),
            Some("cm") | None => Ok(Resolution::Cm { horizontal, vertical }),
            Some(bad) => Err(report(InvalidAttr)).attach_printable(format!("Invalid unit attribute: expected one of [inch, cm]; found {bad}")),
        }
    }
}
