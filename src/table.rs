use std::collections::HashMap;

use error_stack::{ Report, report, Result, ResultExt };
use libxml::{
    readonly::RoNode,
    xpath::Context as XpathContext
};
use ndarray::{Array2, ArrayView1};
use parse_int::parse as parse_auto_radix;
use crate::{
    data_block::DataBlock, error::{ParseNodeError, ParseNodeErrorKind::{self, *}}, MaybeReference, PropertyType, PropertyValue, ReadOptions
};

const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("Table", kind)
}
fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}

/// Describes the content of a [Table] column
#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Field {
    /// Machine-readable title for this field
    id: String,
    /// What type of data to expect in this column
    r#type: PropertyType,
    /// Format specifier
    format: Option<String>,
    /// Human-readable title for this field
    header: Option<String>,
}
impl Field {
    pub(crate) fn parse_node(node: RoNode, _xpath: &XpathContext, _opts: &ReadOptions) -> Result<Self, ParseNodeError> {
        let mut attrs = node.get_attributes();

        let id = attrs.remove("id")
            .ok_or(context(MissingAttr))
            .attach_printable("Missing id attribute")?;

        let r#type = attrs.remove("type")
            .ok_or(report(MissingAttr))
            .attach_printable("Missing type attribute")?
            .parse::<PropertyType>()
            .change_context(context(InvalidAttr))
            .attach_printable("Invalid PropertyType attribute")?;

        let format = attrs.remove("format");

        let header = attrs.remove("header");

        for remaining in attrs.into_iter() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }
        for remaining in node.get_child_nodes() {
            tracing::warn!("Ignoring unrecognized child node <{}>", remaining.get_name());
        }

        Ok(Self {
            id,
            r#type,
            format,
            header,
        })
    }
}

/// A declaration of what data to expect in each column
///
/// Tables are laid out such that each column has cells with only one shared data type.
/// The structure element which columns have which data types, and associated details such as
/// human-friendly and machine-friendly labels for each column, and format strings to specify
/// how this data should be printed (where it should be aligned in the cell, how many decimal places to render, etc).
/// Note that format strings are specified per-column, and cannot vary across cells within a column.
#[derive(Clone, Debug)]
pub struct Structure {
    fields: Vec<Field>,
    id_lookup: HashMap<String, usize>,
}
impl Structure {
    pub(crate) fn parse_node(node: RoNode, xpath: &XpathContext, opts: &ReadOptions) -> Result<Self, ParseNodeError> {
        for remaining in node.get_attributes() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }
        let mut fields = vec![];
        for child in node.get_child_nodes() {
            match child.get_name().as_str() {
                "Field" => fields.push(Field::parse_node(child, xpath, opts)?),
                other => tracing::warn!("Ignoring unrecognized child node <{}>", other),
            }
        }

        let mut id_lookup = HashMap::new();
        for (i, f) in fields.iter().enumerate() {
            id_lookup.insert(f.id.clone(), i);
        }

        Ok(Self {
            fields,
            id_lookup,
        })
    }
}

/// An XISF [Table](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#__XISF_Core_Elements_:_Table_Core_Element__) core element
///
/// Tables are defined as a series of [PropertyValue]s organized into rows and columns, along with an associated [Structure] element defining the table's layout.
/// Tables must be completely populated, that is, each row must have the same number of elements as all other rows, and same for columns.
#[derive(Clone, Debug)]
pub struct Table {
    id: String,
    caption: Option<String>,
    structure: Structure,
    cells: Array2<PropertyValue>,
    comment: Option<String>,
}
impl Table {
    pub(crate) fn parse_node(node: RoNode, xpath: &XpathContext, opts: &ReadOptions) -> Result<Self, ParseNodeError> {
        let _span_guard = tracing::debug_span!("Table").entered();

        let mut attrs = node.get_attributes();

        let id = attrs.remove("id")
            .ok_or(context(MissingAttr))
            .attach_printable("Missing id attribute")?;

        let caption = attrs.remove("caption");

        let rows = if let Some(val) = attrs.remove("rows") {
            Some(parse_auto_radix::<usize>(val.trim())
                .change_context(context(InvalidAttr))
                .attach_printable("Invalid rows attribute: failed to parse as usize")?)
        } else {
            None
        };

        let columns = if let Some(val) = attrs.remove("rows") {
            Some(parse_auto_radix::<usize>(val.trim())
                .change_context(context(InvalidAttr))
                .attach_printable("Invalid columns attribute: failed to parse as usize")?)
        } else {
            None
        };

        let comment = attrs.remove("comment");

        for remaining in attrs.into_iter() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }

        let mut structure = None;
        let mut cells = vec![];
        let mut actual_rows = 0;
        let mut last_actual_cols = None;

        for mut child in node.get_child_nodes() {
            child = child.follow_reference(xpath).change_context(context(InvalidReference))?;

            match child.get_name().as_str() {
                "Structure" => {
                    structure = Some(Structure::parse_node(child, xpath, opts)?);
                },
                "Row" => {
                    for remaining in child.get_attributes() {
                        tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
                    }
                    let mut actual_cols = 0;
                    for child in child.get_child_nodes() {
                        match child.get_name().as_str() {
                            "Cell" => {
                                cells.push(parse_cell(child, xpath, opts)?);
                                actual_cols += 1;
                            },
                            other => tracing::warn!("Ignoring unrecognized child node <{}>", other),
                        }
                    }

                    if let Some(c) = last_actual_cols {
                        if actual_cols != c {
                            tracing::warn!("Table row {actual_rows} (0-indexed) has a different number of columns than the previous row");
                        }
                    }
                    if let Some(c) = columns {
                        if actual_cols != c {
                            tracing::warn!("Table row {actual_rows} (0-indexed) has a different number of cells than specified by the table's columns attribute");
                        }
                    }
                    last_actual_cols = Some(actual_cols);

                    actual_rows += 1;
                }
                other => tracing::warn!("Ignoring unrecognized child node <{}>", other),
            }
        }

        let structure = structure.ok_or(report(MissingChild))
            .attach_printable("Missing <Structure> child element")?;

        if let Some(c) = columns {
            if c != structure.fields.len() {
                tracing::warn!("Number of fields in table structure ({}) does not match columns attribute ({})", structure.fields.len(), c);
            }
        }
        let columns = columns.unwrap_or(structure.fields.len());

        if let Some(r) = rows {
            if r != actual_rows {
                tracing::warn!("Number of rows actually found ({actual_rows}) does not match rows attribute ({r})")
            }
        }
        let rows = rows.unwrap_or(actual_rows);

        let len = cells.len();
        let cells = Array2::from_shape_vec((rows, columns), cells)
            .change_context(context(InvalidChild))
            .attach_printable(format!("Incorrect number of cells ({len}) for a {rows}x{columns} table"))?;

        Ok(Self {
            id,
            caption,
            structure,
            cells,
            comment,
        })
    }

    /// Returns the table's locally-unique, machine-friendly ID
    ///
    /// IDs must be unique among other tables and properties associated with the same parent element, but are not necessarily globally unique.
    #[inline]
    pub fn id(&self) -> &str {
        self.id.as_str()
    }

    /// Returns the number of fields specified in this table's [Structure] element
    #[inline]
    pub fn num_fields(&self) -> usize {
        self.structure.fields.len()
    }

    /// Returns a specific field from this table's [Structure] element, stored at index `i` (0-indexed)
    ///
    /// # Panics
    /// If `i` is outside the range `0..num_fields()`
    #[inline]
    pub fn field(&self, i: usize) -> &Field {
        &self.structure.fields[i]
    }

    /// An iterator over the [Field]s of this table
    #[inline]
    pub fn fields(&self) -> impl Iterator<Item = &Field> {
        self.structure.fields.iter()
    }

    /// A caption or label to be rendered alongside the table
    ///
    /// Not to be confused with [comment()][Self::comment()], which is not usually as easily visible
    #[inline]
    pub fn caption(&self) -> Option<&str> {
        self.caption.as_deref()
    }

    /// Returns the number of rows in this table
    #[inline]
    pub fn num_rows(&self) -> usize {
        self.cells.nrows()
    }

    /// Returns a specific row from the table, stored at index `r` (0-indexed)
    ///
    /// # Panics
    /// If `r` is outside the range `0..num_rows()`
    #[inline]
    pub fn row(&self, r: usize) -> ArrayView1<'_, PropertyValue> {
        self.cells.row(r)
    }

    /// Returns an iterator over all the rows in this table
    #[inline]
    pub fn rows(&self) -> impl Iterator<Item = ArrayView1<'_, PropertyValue>> {
        self.cells.rows().into_iter()
    }

    /// Returns the number of columns in this table
    #[inline]
    pub fn num_columns(&self) -> usize {
        self.cells.ncols()
    }

    /// Returns a specific column from the table, stored at index `c` (0-indexed)
    ///
    /// # Panics
    /// If `c` is outside the range `0..num_columns()`
    #[inline]
    pub fn column(&self, c: usize) -> ArrayView1<'_, PropertyValue> {
        self.cells.column(c)
    }

    /// Returns an iterator over all columns in this table
    ///
    /// <div class="warning">
    ///
    /// [PropertyValue] types do not contain complete type information, so the type should be
    /// validated through the [fields()][Self::fields()] before attempting to parse it
    ///
    /// </div>
    #[inline]
    pub fn columns(&self) -> impl Iterator<Item = ArrayView1<'_, PropertyValue>> {
        self.cells.columns().into_iter()
    }

    /// Returns the index of the column with the given ID, if one exists
    #[inline]
    pub fn find_column(&self, id: &str) -> Option<usize> {
        self.structure.id_lookup.get(id).cloned()
    }

    /// Returns a single cell from row `r` and column `c`
    ///
    /// If `r` or `c` is outside the range of the table, returns `None` instead
    #[inline]
    pub fn cell(&self, r: usize, c: usize) -> Option<&PropertyValue> {
        self.cells.get((r, c))
    }

    /// Supplementary information about the table, such as intended use, source, attributions, etc
    ///
    /// The use of this field is *discouraged* by the XISF spec in favor of assigning a descriptive ID and caption.
    /// Personally I think discouraged is overly strong wording, but do avoid duplicating info that is specified elsewhere.
    #[inline]
    pub fn comment(&self) -> Option<&str> {
        self.comment.as_deref()
    }
}

fn parse_cell(node: RoNode, _xpath: &XpathContext, _opts: &ReadOptions) -> Result<PropertyValue, ParseNodeError> {
    let mut attrs = node.get_attributes();

    let value = if let Some(block) = DataBlock::parse_node(node, "Cell", &mut attrs)? {
        if let Some(len) = attrs.remove("length") {
            PropertyValue::Vector {
                len: parse_auto_radix::<usize>(len.trim())
                    .change_context(context(InvalidAttr))
                    .attach_printable("Invalid length attribute: failed to parse as usize")?,
                block
            }
        } else if let Some(rows) = attrs.remove("rows") {
            if let Some(cols) = attrs.remove("columns") {
                PropertyValue::Matrix {
                    rows: parse_auto_radix::<usize>(rows.trim())
                        .change_context(context(InvalidAttr))
                        .attach_printable("Invalid rows attribute: failed to parse as usize")?,
                    columns: parse_auto_radix::<usize>(cols.trim())
                        .change_context(context(InvalidAttr))
                        .attach_printable("Invalid columns attribute: failed to parse as usize")?,
                    block
                }
            } else {
                return Err(report(MissingAttr))
                    .attach_printable("Missing columns attribute: matrix-type cells must have rows and columns attributes");
            }
        } else {
            PropertyValue::DataBlock(block)
        }
    } else {
        let value = attrs.remove("value")
            .ok_or(context(MissingAttr))
            .attach_printable("Cells with no data block must have a plaintext value")?;

        for remaining in node.get_child_nodes() {
            tracing::warn!("Ignoring unrecognized child node <{}>", remaining.get_name());
        }

        PropertyValue::Plaintext(value)
    };

    for remaining in attrs.into_iter() {
        tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
    }

    Ok(value)
}