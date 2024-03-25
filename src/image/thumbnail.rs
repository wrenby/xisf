use std::fmt::Debug;

use error_stack::{Result, Report, report, ResultExt};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};

use crate::{ReadOptions, error::{ParseNodeError, ParseNodeErrorKind}};

use super::*;

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

/// A variant of [`Image`] with some additional restrictions
/// - Thumbnails must be two-dimensional
/// - Thumbnail color space must be RGB or grayscale, with a single optional alpha channel
/// - Thumbnail samples must be either UInt8 or UInt16
/// - Thumbnails may not have a `bounds` attribute, and must use the full range of their sample type
/// - Thumbnails may not have thumbnail  or color filter array elements
/// - Thumbnails must be pre-demosaiced, and may not have a ColorFilterArray element
///
/// As a guideline, thumbnails should not be larger than 1024 pixels on either dimension.
#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct Thumbnail(ImageBase);
impl Thumbnail {
    pub(crate) fn parse_node(node: RoNode, xpath: &XpathContext, opts: &ReadOptions) -> Result<Thumbnail, ParseNodeError> {
        let image = super::parse_image::<Self>(node, xpath, opts)?;

        if image.num_dimensions() != 2 {
            Err(report(InvalidAttr)).attach_printable("Thumbnail images must be two-dimensional")
        } else if !image.sample_format().ok_for_thumbnails() {
            Err(report(InvalidAttr)).attach_printable("Thumbnail images must have either UInt8 or UInt16 samples")
        } else if !image.color_space().ok_for_thumbnails() {
            Err(report(InvalidAttr)).attach_printable("Thumbnail images must be either RGB or Grayscale color space")
        } else if image.num_alpha_channels() > 1 {
            Err(report(InvalidAttr)).attach_printable("Thumbnail images are allowed a max of 1 alpha channel")
        } else if image.bounds().is_some() {
            Err(report(InvalidAttr)).attach_printable("Thumbnail elements may not have a bounds attribute")
        } else {
            Ok(Self(image.base))
        }
    }

    delegate::delegate! {
        to self.0 {
            /// The number of dimensions in this image
            ///
            /// The channel axis is not considered a dimension
            pub fn num_dimensions(&self) -> usize;
            /// A slice of dimension sizes, in row-major order
            ///
            /// For a 2D image, this means height is before width.
            /// The channel axis is not considered a dimension.
            pub fn dimensions(&self) -> &[usize];
            /// The total number of channels in this image, both color and alpha
            pub fn num_channels(&self) -> usize;
            /// Called nominal channels in the spec, this is the number of channels required to represent
            /// all components of the image's color space (1 for grayscale, 3 for RGB or L\*a\*b\*)
            pub fn num_color_channels(&self) -> usize;
            /// Any channel after what's needed for the image's color space (1 for grayscale, 3 for RGB or L\*a\*b\*) is considered an alpha channel
            pub fn num_alpha_channels(&self) -> usize;
            /// Returns the sample format
            pub fn sample_format(&self) -> SampleFormat;
            /// Returns the image type (bias, dark, flat, light, etc)
            pub fn image_type(&self) -> Option<ImageType>;
            /// Returns the pixel sample's layout in memory
            pub fn pixel_layout(&self) -> PixelStorage;
            /// Returns the color space
            pub fn color_space(&self) -> ColorSpace;
            /// Returns the offset (AKA pedestal)
            ///
            /// An offset is a value added to all pixel samples, sometimes necessary to
            /// ensure positive data from the sensor in the presence of noise
            pub fn offset(&self) -> f64;
            /// Returns a simple transformation (90-degree rotations and an a reflection) which should be applied before the image is displayed
            pub fn orientation(&self) -> Option<Orientation>;
            /// Returns the ID, if one exists
            ///
            /// An ID is a sequence of ASCII characters that may be used to identify the image to the end user,
            /// and could be thought of as a name tag. Must satisfy the regex `[_a-zA-Z][_a-zA-Z0-9]*`.
            pub fn id(&self) -> Option<&str>;
            /// Returns the UUID, if one exists
            pub fn uuid(&self) -> Option<Uuid>;
            /// Reads data from the image's [data block](DataBlock) and packs it into an image type
            pub fn read_data(&self, ctx: &Context) -> Result<DynImageData, ReadDataBlockError>;
            /// Returns a container of all the image's XISF properties
            pub fn properties(&self) -> &Properties;
            /// Returns a container of all the image's FITS keyword records
            pub fn fits_header(&self) -> &FitsHeader;
            /// Returns a reference to the embedded ICC profile, if one exists.
            /// If the returned value is `Some`, obtain the profile data by calling `read_data()` on the contained value.
            /// Note: `read_data()` just returns a `Vec<u8>`; consider the `lcms2` crate if you need to actually decode it.
            pub fn icc_profile(&self) -> Option<&ICCProfile>;
            /// Returns a reference to the RGB working space, if one is specified.
            /// If none is specified, the default is the sRGB color space, relative to the D50 standard illuminant.
            /// Consider using [`Option::unwrap_or_default()`] on the result.
            pub fn rgb_working_space(&self) -> Option<&RGBWorkingSpace>;
            /// Returns a reference to the display function, if one is specified.
            /// If none is specified, the default is the identity function.
            /// Although the identity display function is the [`Default`], using [`Option::unwrap_or_default()`] on the result
            /// would likely be unwise for most cases, as applying the identity function would result in unnecessary computation.
            pub fn display_function(&self) -> Option<&DisplayFunction>;
            /// Returns the pixel density of this image, in pixels-per-inch or pixels-per-centimeter
            /// If none is specified, the default is 72 PPI.
            /// Consider using [`Option::unwrap_or_default()`] on the result
            pub fn pixel_density(&self) -> Option<&Resolution>;
        }
    }
}

impl ParseImage for Thumbnail {
    const TAG_NAME: &'static str = "Thumbnail";
}
