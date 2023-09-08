use error_stack::{Report, report, ResultExt};
use libxml::readonly::RoNode;

use crate::{
    data_block::{ByteOrder, Context, DataBlock},
    error::{ParseNodeError, ParseNodeErrorKind::{self, *}, ReadDataBlockError},
};

fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}
const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("ICCProfile", kind)
}

#[derive(Clone, Debug)]
pub struct ICCProfile {
    data_block: DataBlock,
}
impl ICCProfile {
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, Report<ParseNodeError>> {
        let _span_guard = tracing::debug_span!("ICCProfile");
        let mut attrs = node.get_attributes();
        let children = node.get_child_nodes();

        if let Some(_byte_order) = attrs.remove("byteOrder") {
            Err(report(InvalidAttr)).attach_printable("byteOrder attribute is not allowed on ICCProfile elements as they are always big-endian")
        } else {
            let mut data_block = DataBlock::parse_node(node, "ICCProfile", &mut attrs)?
                .ok_or(context(MissingAttr))
                .attach_printable("Missing location attribute: ICCProfile elements must have a data block")?;
            data_block.byte_order = ByteOrder::Big;

            for remaining in attrs.into_iter() {
                tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
            }
            for child in children {
                tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
            }

            Ok(Self {
                data_block,
            })
        }
    }

    pub fn read_data(&self, ctx: &Context) -> Result<Vec<u8>, Report<ReadDataBlockError>> {
        let mut buf = vec![];
        self.data_block.verify_checksum(ctx)?;
        self.data_block.decompressed_bytes(ctx)?
            .read_to_end(&mut buf)
            .change_context(ReadDataBlockError::IoError)?;
        Ok(buf)
    }
}