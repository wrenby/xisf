use std::{collections::HashMap, ops::Deref};

use error_stack::{Result, ResultExt};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};

use crate::{
    property::{PropertyContent, Property},
    error::{ParseNodeError, ParseNodeErrorKind::{self, *}},
    reference::MaybeReference,
};

const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("Metadata", kind)
}

#[derive(Clone, Debug)]
pub(crate) struct Metadata {
    properties: HashMap<String, PropertyContent>,
}
impl Deref for Metadata {
    type Target = HashMap<String, PropertyContent>;

    fn deref(&self) -> &Self::Target {
        &self.properties
    }
}
impl Metadata {
    pub(crate) fn parse_node(node: RoNode, xpath: &XpathContext) -> Result<Self, ParseNodeError> {
        let _span_guard = tracing::debug_span!("Metadata");

        for remaining in node.get_attributes() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }

        let mut properties = HashMap::new();
        for mut child in node.get_child_nodes() {
            child = child.follow_reference(xpath).change_context(context(InvalidReference))?;

            match child.get_name().as_str() {
                "Property" => {
                    let prop = Property::parse_node(child)?;
                    if !prop.id.starts_with("XISF:") {
                        tracing::warn!("Metadata properties should have IDs that start with \"XISF:\"");
                    }
                    if properties.insert(prop.id.clone(), prop.content).is_some() {
                        tracing::warn!("Duplicate property found with id {} -- discarding the previous one", prop.id);
                    }
                }
                _ => tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name()),
            }
        }

        Ok(Self {
            properties,
        })
    }
}
