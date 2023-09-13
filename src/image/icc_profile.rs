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

/// A data block storing an [ICC color profile](https://www.color.org/specification/ICC.1-2022-05.pdf)
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

    /// Reads the ICC profile into a buffer
    ///
    /// Just an array of bytes, with no byte-order-swapping because ICC Profiles are always in big endian.
    /// To actually read the profile, consider the [`lcms2`](crates.io/crates/lcms2) crate.
    pub fn read_data(&self, ctx: &Context) -> Result<Vec<u8>, Report<ReadDataBlockError>> {
        let mut buf = vec![];
        self.data_block.verify_checksum(ctx)?;
        self.data_block.decompressed_bytes(ctx)?
            .read_to_end(&mut buf)
            .change_context(ReadDataBlockError::IoError)?;

        // byte 44 bit 1 is the bitflag for embedded profiles, which the XISF spec says must be set
        const PROFILE_FLAGS: usize = 44;
        if let Some(profile) = buf.get(PROFILE_FLAGS) {
            const EMBEDDED: u8 = 1 << 7;
            if profile & EMBEDDED == 0 {
                tracing::warn!("ICC Profiles in XISF files should have the embedded flag set in the profile's header");
            }
        }
        Ok(buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{self, File};
    use std::io::BufReader;
    use crate::data_block::Location;
    use libxml::parser::Parser;

    #[test]
    fn parse_node() {
        let attached = r#"<ICCProfile location="attachment:4096:8192" />"#;

        let xml = Parser::default().parse_string(attached.as_bytes()).unwrap();
        let icc = ICCProfile::parse_node(xml.get_root_readonly().unwrap()).unwrap();
        assert_eq!(icc.data_block.byte_order, ByteOrder::Big);
        assert_eq!(icc.data_block.location, Location::Attachment { position: 4096, size: 8192 });

        // ICCProfile elements cannot have a byteOrder attribute, even when that attribute is big
        let byte_order = r#"<ICCProfile byteOrder="big" location="attachment:4096:8192" />"#;
        let xml = Parser::default().parse_string(byte_order.as_bytes()).unwrap();
        let err = ICCProfile::parse_node(xml.get_root_readonly().unwrap()).unwrap_err();
        assert_eq!(err.current_context().kind, ParseNodeErrorKind::InvalidAttr);

        let no_block = r#"<ICCProfile />"#;
        let xml = Parser::default().parse_string(no_block.as_bytes()).unwrap();
        let err = ICCProfile::parse_node(xml.get_root_readonly().unwrap()).unwrap_err();
        assert_eq!(err.current_context().kind, ParseNodeErrorKind::MissingAttr);
    }

    #[test]
    fn read() {
            let raw = fs::read("tests/files/sRGB-elle-V4-g22.icc").unwrap();
            let file = File::open("tests/files/sRGB-elle-V4-g22.icc").unwrap();
            let size = file.metadata().unwrap().len();
            let attachment = DataBlock {
                location: Location::Attachment { position: 0, size: size },
                byte_order: ByteOrder::Little, // doesn't matter, since we're using raw_bytes
                checksum: None,
                compression: None,
            };
            let ctx = Context::monolithic(BufReader::new(file));
            let icc = ICCProfile {
                data_block: attachment,
            };
            let buf = icc.read_data(&ctx).unwrap();
            let profile = lcms2::Profile::new_icc(&buf).unwrap();
            let bytes = profile.icc().unwrap();
            // ICC profiles have 4 bytes in the header to declare what platform the encoder is running on -- ignore them
            assert_eq!(&bytes[0..40], &buf[0..40]);
            assert_eq!(&bytes[44..], &buf[44..]);
            assert_eq!(raw, buf);
    }
}
