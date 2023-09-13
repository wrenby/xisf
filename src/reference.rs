use error_stack::{report, ResultExt, Report};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};
use tracing::{debug_span, field::Empty};

use crate::error::ReferenceError;

/// Resolves `<Reference>` elements with an XPATH search
///
/// `<Reference>` XML elements are never given any kind of form, and are resolved as they are encountered like a symlink.
/// As a consequence, referenced elements will be unnecessarily duplicated in memory:
/// once reading the original, and once reading the reference. This was deemed a reasonable trade-off
/// for ease of use since the bulk of the memory storage is stored in data blocks, which can still be deduplicated.
pub(crate) trait MaybeReference: Sized {
    /// If this node is a reference, returns the node it points to.
    /// If this node is not a reference, this is a no-op.
    fn follow_reference(self, xpath: &XpathContext) -> Result<Self, Report<ReferenceError>>;
}

impl MaybeReference for RoNode {
    fn follow_reference(self, xpath: &XpathContext) -> Result<RoNode, Report<ReferenceError>> {
        if self.get_name().as_str() == "Reference" {
            let _span_guard = debug_span!("Reference", "ref" = Empty).entered();
            if let Some(target) = self.get_attribute("ref") {
                _span_guard.record("ref", target.as_str());
                if is_valid_id(target.as_str()) {
                    // acts as both validation and input sanitization for the xpath expression
                    let expression = format!("/*[local-name()='xisf']//*[@uid='{}']", target);
                    // TODO: is this guaranteed to succeed with a valid uid?
                    let results = xpath.evaluate(&expression)
                        .map_err(|_| report!(ReferenceError::NotFound))
                        .attach_printable("XPATH search failed")?;
                    match results.get_readonly_nodes_as_vec().as_slice() {
                        [] => Err(report!(ReferenceError::NotFound)).attach_printable(
                            format!("No element found in file with uid \"{}\"", target)),
                        &[node] => {
                            if node.get_name().as_str() == "Reference" {
                                Err(report!(ReferenceError::Recursive))
                                    .attach_printable("Reference elements may not reference other reference elements")
                            } else {
                                Ok(node)
                            }
                        }
                        bad => Err(report!(ReferenceError::Ambiguous)).attach_printable(
                            format!("Ambiguous reference: found {} elements which share the uid \"{}\"", bad.len(), target)),
                    }
                } else {
                    Err(report!(ReferenceError::InvalidUid))
                        .attach_printable("ref attribute is not a valid uid: must match the regex [_a-zA-Z][_a-zA-Z0-9]*")
                }
            } else {
                Err(report!(ReferenceError::MissingRef)).attach_printable("Missing ref attribute")
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

#[cfg(test)]
mod tests {
    use super::*;
    use libxml::parser::Parser;

    #[test]
    fn valid_id() {
        // examples from the XISF spec
        assert_eq!(is_valid_id("MyFirstProperty"), true);
        assert_eq!(is_valid_id("mySecondOne234"), true);
        assert_eq!(is_valid_id("_this_1_is_a_test"), true);
        assert_eq!(is_valid_id("Namespace:Property"), false);
        assert_eq!(is_valid_id("foo:bar:Foo2_Bar3"), false);
        // custom examples
        assert_eq!(is_valid_id("5tarts_with_number"), false);
        assert_eq!(is_valid_id("hyphens-instead-of-underscores"), false);
    }

    #[test]
    fn follow_reference() {
        let doc = r#"<xisf>
            <Image uid="good" />
            <Reference ref="good" />
            <haystack>
                <haystack>
                    <Image uid="needle" />
                    <Image uid="ambiguous" />
                </haystack>
            </haystack>
            <Reference ref="needle" />
            <Reference />
            <Reference ref="invalid uid" />
            <Reference ref="not_found" />
            <Reference uid="circular" ref="circular" />
            <Reference ref="recursive" />
            <Image uid="ambiguous" />
            <Reference uid="recursive" ref="ambiguous" />
        </xisf>"#;

        let xml = Parser::default().parse_string(doc.as_bytes()).expect("parse XML");
        let xpath = &XpathContext::new(&xml).expect("init xpath");
        let root = xml.get_root_readonly().expect("get root");
        let mut children = root.get_child_nodes().into_iter().filter(|n| n.get_name().as_str() == "Reference");

        // should not have any affect on non-reference nodes
        assert_eq!(root.follow_reference(xpath).unwrap(), root);

        let good = children.next().expect("good");
        assert!(good.follow_reference(xpath).is_ok());

        let needle = children.next().expect("needle");
        assert!(needle.follow_reference(xpath).is_ok());

        let no_ref = children.next().expect("no_ref");
        let err = no_ref.follow_reference(xpath).unwrap_err();
        assert_eq!(err.current_context(), &ReferenceError::MissingRef);

        let invalid = children.next().expect("invalid");
        let err = invalid.follow_reference(xpath).unwrap_err();
        assert_eq!(err.current_context(), &ReferenceError::InvalidUid);

        let not_found = children.next().expect("not found");
        let err = not_found.follow_reference(xpath).unwrap_err();
        assert_eq!(err.current_context(), &ReferenceError::NotFound);

        let circular = children.next().expect("circular");
        let err = circular.follow_reference(xpath).unwrap_err();
        assert_eq!(err.current_context(), &ReferenceError::Recursive);

        let recursive = children.next().expect("recursive");
        let err = recursive.follow_reference(xpath).unwrap_err();
        assert_eq!(err.current_context(), &ReferenceError::Recursive);

        let ambiguous = children.next().expect("ambiguous");
        let err = ambiguous.follow_reference(xpath).unwrap_err();
        assert_eq!(err.current_context(), &ReferenceError::Ambiguous);

        assert_eq!(children.next(), None);
    }
}
