use error_stack::{Report, report, ResultExt};
use libxml::readonly::RoNode;

use crate::{data_block::{DataBlock, ByteOrder}, error::{ParseNodeError, ReadDataBlockError}, XISF};

#[derive(Clone, Debug)]
pub struct ICCProfile {
    data_block: DataBlock,
}
impl ICCProfile {
    pub(crate) fn parse_node(node: RoNode) -> Result<Self, Report<ParseNodeError>> {
        const CONTEXT: ParseNodeError = ParseNodeError("ICCProfile");
        let _span_guard = tracing::debug_span!("ICCProfile");
        let mut attrs = node.get_attributes();
        let children = node.get_child_nodes();

        if let Some(_byte_order) = attrs.remove("byteOrder") {
            Err(report!(CONTEXT)).attach_printable("byteOrder attribute is not allowed on ICCProfile elements as they are always big-endian")
        } else if let Some(mut data_block) = DataBlock::parse_node(node, CONTEXT, &mut attrs)? {
            data_block.byte_order = ByteOrder::Big;

            for child in children {
                tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
            }

            Ok(Self {
                data_block,
            })
        } else {
            Err(report!(CONTEXT)).attach_printable("Missing location attribute: ICCProfile elements must have a data block")
        }
    }

    pub fn read_data(&self, root: &XISF) -> Result<Vec<u8>, Report<ReadDataBlockError>> {
        let mut buf = vec![];
        self.data_block.decompressed_bytes(root)?
            .read_to_end(&mut buf)
            .change_context(ReadDataBlockError)?;
        Ok(buf)
    }
}