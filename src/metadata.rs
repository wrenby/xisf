use error_stack::{Report, report, ResultExt};
use libxml::readonly::RoNode;

use crate::error::ParseNodeError;



#[derive(Clone, Debug)]
pub struct FitsKeyword {
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
            .attach_printable("Missing name attribute")?;

        let value = attrs.remove("value")
            .ok_or(report!(CONTEXT))
            .attach_printable("Missing value attribute")?;


        let comment = attrs.remove("comment")
            .ok_or(report!(CONTEXT))
            .attach_printable("Missing comment attribute")?;

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