use std::collections::HashMap;

use error_stack::{report, Result, ResultExt};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};

use crate::{
    Context,
    property::{FromProperty, PropertyContent, Property},
    error::{ReadPropertyError, ParseNodeError, ParseNodeErrorKind::{self, *}},
    reference::MaybeReference,
};

const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("Metadata", kind)
}

#[derive(Clone, Debug)]
pub struct Metadata {
    properties: HashMap<String, PropertyContent>,
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

    /// Returns true iff an XISF property is present with the given ID
    pub fn has_property(&self, id: impl AsRef<str>) -> bool {
        self.properties.contains_key(id.as_ref())
    }

    /// Attempts to parse an XISF property with the given ID as type T
    ///
    /// To read a value and comment pair, use the pattern `let (value, comment) = properties.parse_property("ID", &xisf)?;`
    pub fn parse_property<T: FromProperty>(&self, id: impl AsRef<str>, ctx: &Context) -> Result<T, ReadPropertyError> {
        let content = self.properties.get(id.as_ref())
            .ok_or(report!(ReadPropertyError::KeyNotFound))?;
        T::from_property(&content, ctx)
            .change_context(ReadPropertyError::InvalidFormat)
    }
    /// Returns the raw content of the XISF property matching the given ID`
    pub fn raw_property(&self, id: impl AsRef<str>) -> Option<&PropertyContent> {
        self.properties.get(id.as_ref())
    }
    /// Iterates through all XISF properties as (id, type+value+comment) tuples,
    /// in the order they appear in file, returned as raw unparsed strings/data blocks.
    pub fn all_raw_properties(&self) -> impl Iterator<Item = (&String, &PropertyContent)> {
        self.properties.iter()
    }
}