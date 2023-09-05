use std::{ops::Deref, fmt::Debug};

use error_stack::{Result, Report, report, ResultExt};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};

use crate::{ReadOptions, error::{ParseNodeError, ParseNodeErrorKind::{self, *}}};

use super::{ImageBase, ParseImage, Image, SampleFormat, ColorSpace};

trait OkForThumbnails {
    fn ok_for_thumbnails(&self) -> bool;
}
impl OkForThumbnails for SampleFormat {
    fn ok_for_thumbnails(&self) -> bool {
        match self {
            SampleFormat::UInt8 | SampleFormat::UInt16 => true,
            _ => false,
        }
    }
}
impl OkForThumbnails for ColorSpace {
    fn ok_for_thumbnails(&self) -> bool {
        match self {
            ColorSpace::RGB | ColorSpace::Gray => true,
            _ => false,
        }
    }
}

fn report(kind: ParseNodeErrorKind) -> Report<ParseNodeError> {
    report!(context(kind))
}
const fn context(kind: ParseNodeErrorKind) -> ParseNodeError {
    ParseNodeError::new("Thumbnail", kind)
}

#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct Thumbnail(ImageBase);
impl Thumbnail {
    pub(crate) fn parse_node(node: RoNode, xpath: &XpathContext, opts: &ReadOptions) -> Result<Thumbnail, ParseNodeError> {
        super::parse_image::<Self>(node, xpath, opts)?.try_into()
    }
}
impl Deref for Thumbnail {
    type Target = ImageBase;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl TryFrom<Image> for Thumbnail {
    type Error = Report<ParseNodeError>;

    fn try_from(image: Image) -> std::result::Result<Self, Self::Error> {
        if image.num_dimensions() != 2 {
            Err(report(InvalidAttr)).attach_printable("Thumbnail images must be two-dimensional")
        } else if !image.sample_format.ok_for_thumbnails() {
            Err(report(InvalidAttr)).attach_printable("Thumbnail images must have either UInt8 or UInt16 samples")
        } else if !image.color_space.ok_for_thumbnails() {
            Err(report(InvalidAttr)).attach_printable("Thumbnail images must be either RGB or Grayscale color space")
        } else if image.num_alpha_channels() > 1 {
            Err(report(InvalidAttr)).attach_printable("Thumbnail images are allowed a max of 1 alpha channel")
        } else if image.bounds.is_some() {
            Err(report(InvalidAttr)).attach_printable("Thumbnail elements may not have a bounds attribute")
        } else {
            Ok(Self(image.base))
        }
    }
}

impl ParseImage for Thumbnail {
    const TAG_NAME: &'static str = "Thumbnail";
}