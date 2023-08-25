use error_stack::{report, ResultExt, Report};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};

use crate::error::ReferenceError;

pub(crate) trait MaybeReference: Sized {
    /// If this node is a reference, returns the node it points to
    /// If this node is not a reference, this is a no-op
    fn follow_reference(self, xpath: &XpathContext) -> Result<Self, Report<ReferenceError>>;
}

impl MaybeReference for RoNode {
    fn follow_reference(self, xpath: &XpathContext) -> Result<RoNode, Report<ReferenceError>> {
        if self.get_name().as_str() == "Reference" {
            let _span_guard = tracing::debug_span!("Reference").entered();
            if let Some(target) = self.get_attribute("ref") {
                if is_valid_id(target.as_str()) {
                    // acts as both validation and input sanitization for the xpath expression
                    let expression = format!("/*[local-name()='xisf']//[@uid='{}']", target);
                    let results = xpath.evaluate(&expression)
                        .map_err(|_| report!(ReferenceError))
                        .attach_printable("XPATH search failed")?;
                    match results.get_readonly_nodes_as_vec().as_slice() {
                        [] => Err(report!(ReferenceError)).attach_printable(
                            format!("No element found in file with uid \"{}\"", target)),
                        &[node] => {
                            if node.get_name().as_str() == "Reference" {
                                Err(report!(ReferenceError))
                                    .attach_printable("Reference elements may not reference other reference elements")
                            } else {
                                Ok(node)
                            }
                        }
                        bad => Err(report!(ReferenceError)).attach_printable(
                            format!("Ambiguous reference: found {} elements which share the uid \"{}\"", bad.len(), target)),
                    }
                } else {
                    Err(report!(ReferenceError))
                        .attach_printable("ref attribute is not a valid uid: must match the regex [_a-zA-Z][_a-zA-Z0-9]*")
                }
            } else {
                Err(report!(ReferenceError)).attach_printable("Missing ref attribute")
            }
        } else {
            Ok(self)
        }
    }
}

/// Returns true iff `s` satisfies the regex [_a-zA-Z][_a-zA-Z0-9]*
/// Gets used for Reference uids, Image ids, and XISF Property IDs
pub(crate) fn is_valid_id(s: impl AsRef<str>) -> bool {
    let s = s.as_ref();
    let first_ok = s.starts_with(|c: char| c == '_' || c.is_alphabetic());
    first_ok && s[1..].chars().all(|c| c.is_alphanumeric() || c == '_')
}