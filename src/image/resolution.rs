use error_stack::{Result, report, Report, ResultExt};
use libxml::readonly::RoNode;

use crate::error::{ParseNodeError, ParseNodeErrorKind::{self, *}};

fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}
const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("Resolution", kind)
}

#[derive(Clone, Debug)]
pub enum Resolution {
    Inch { horizontal: f32, vertical: f32 },
    Cm { horizontal: f32, vertical: f32 },
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