//! Anything related to [`Image`]

use std::{
    any::TypeId,
    io::Read,
    fmt,
    str::FromStr, ops::Deref, collections::HashMap,
};

use byteorder::{ReadBytesExt, LE, BE};
use error_stack::{Report, report, Result, ResultExt};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};
use ndarray::{ArrayD, IxDyn};
use num_complex::Complex;
use ordered_multimap::ListOrderedMultimap;
use parse_int::parse as parse_auto_radix;
use strum::{Display, EnumString, EnumVariantNames, VariantNames};
use uuid::Uuid;
use crate::{
    data_block::{ByteOrder, Context, DataBlock},
    error::{ReadDataBlockError, ParseValueError, ReadFitsKeyError, ParseNodeErrorKind::*, ReadPropertyError},
    is_valid_id,
    MaybeReference,
    ParseNodeError,
    property::{Property, PropertyContent, FromProperty},
    ReadOptions,
};

mod image_data;
pub use image_data::*;

mod fits_keyword;
pub use fits_keyword::*;

mod icc_profile;
pub use icc_profile::*;

mod rgb_working_space;
pub use rgb_working_space::*;

mod display_function;
pub use display_function::*;

mod color_filter_array;
pub use color_filter_array::*;

mod resolution;
pub use resolution::*;

mod thumbnail;
pub use thumbnail::*;

/// Shared components of [`Image`] and [`Thumbnail`] elements
///
/// Any public field or function of [`ImageBase`] is transparently accessible from
/// [`Image`]s or [`Thumbnail`]s through the use of [`Deref`] coercion
#[derive(Clone, Debug)]
pub struct ImageBase {
    data_block: DataBlock,
    geometry: Vec<usize>,
    sample_format: SampleFormat,

    image_type: Option<ImageType>,
    pixel_storage: PixelStorage,
    color_space: ColorSpace,
    offset: f64,
    orientation: Option<Orientation>,
    id: Option<String>,
    uuid: Option<Uuid>,

    properties: HashMap<String, PropertyContent>,
    fits_header: ListOrderedMultimap<String, FitsKeyContent>,
    icc_profile: Option<ICCProfile>,
    rgb_working_space: Option<RGBWorkingSpace>,
    display_function: Option<DisplayFunction>,
    resolution: Option<Resolution>,
}
impl ImageBase {
    /// The number of dimensions in this image
    ///
    /// The channel axis is not considered a dimension
    #[inline]
    pub fn num_dimensions(&self) -> usize {
        self.geometry.len() - 1
    }

    /// A slice of dimension sizes, in row-major order
    ///
    /// For a 2D image, this means height is before width.
    /// The channel axis is not considered a dimension.
    #[inline]
    pub fn dimensions(&self) -> &[usize] {
        &self.geometry[1..]
    }

    /// The total number of channels in this image, both color and alpha
    #[inline]
    pub fn num_channels(&self) -> usize {
        self.geometry[0]
    }
    /// Called nominal channels in the spec, this is the number of channels required to represent
    /// all components of the image's color space (1 for grayscale, 3 for RGB or L\*a\*b\*)
    #[inline]
    pub fn num_color_channels(&self) -> usize {
        self.color_space.num_channels()
    }
    /// Any channel after what's needed for the image's color space (1 for grayscale, 3 for RGB or L\*a\*b\*) is considered an alpha channel
    #[inline]
    pub fn num_alpha_channels(&self) -> usize {
        self.num_channels() - self.color_space.num_channels()
    }

    /// Returns the sample format
    #[inline]
    pub fn sample_format(&self) -> SampleFormat {
        self.sample_format
    }

    /// Returns the image type (bias, dark, flat, light, etc)
    #[inline]
    pub fn image_type(&self) -> Option<ImageType> {
        self.image_type
    }

    /// Returns the pixel sample's layout in memory
    #[inline]
    pub fn pixel_layout(&self) -> PixelStorage {
        self.pixel_storage
    }

    /// Returns the color space
    #[inline]
    pub fn color_space(&self) -> ColorSpace {
        self.color_space
    }

    /// Returns the offset (AKA pedestal)
    ///
    /// An offset is a value added to all pixel samples, sometimes necessary to
    /// ensure positive data from the sensor in the presence of noise
    #[inline]
    pub fn offset(&self) -> f64 {
        self.offset
    }

    /// Returns a simple transformation (90-degree rotations and an a reflection) which should be applied before the image is displayed
    #[inline]
    pub fn orientation(&self) -> Option<Orientation> {
        self.orientation
    }

    /// Returns the ID, if one exists
    ///
    /// An ID is a sequence of ASCII characters that may be used to identify the image to the end user,
    /// and could be thought of as a name tag. Must satisfy the regex `[_a-zA-Z][_a-zA-Z0-9]*`.
    // TODO: is ID validated while parsing?
    #[inline]
    pub fn id(&self) -> Option<&str> {
        self.id.as_deref()
    }

    /// Returns the UUID, if one exists
    #[inline]
    pub fn uuid(&self) -> Option<Uuid> {
        self.uuid
    }

    /// Reads data from the image's [data block](DataBlock) and packs it into an image type
    // TODO: better handling for CIE L*a*b* images
    pub fn read_data(&self, ctx: &Context) -> Result<DynImageData, ReadDataBlockError> {
        self.data_block.verify_checksum(ctx)?;
        let reader = &mut *self.data_block.decompressed_bytes(ctx)?;

        macro_rules! read_real {
            ($func:ident) => {
                self.read_data_impl(reader,
                    ReadBytesExt::$func::<LE>,
                    ReadBytesExt::$func::<BE>
                ).map(|buf| buf.into_dyn_img(self.pixel_storage))
            }
        }
        macro_rules! read_complex {
            ($func:ident, $t:ty) => {
                {
                    let mut buf;
                    match self.pixel_storage {
                        PixelStorage::Planar => buf = ArrayD::<Complex<$t>>::zeros(IxDyn(&self.geometry[..])),
                        PixelStorage::Normal => {
                            let mut geometry = self.geometry.clone();
                            geometry.rotate_left(1);
                            buf = ArrayD::<Complex<$t>>::zeros(IxDyn(&geometry[..]));
                        },
                    }
                    // this unwrap is safe because:
                    // 1. all owned arrays are contiguous, and
                    // 2. it's in standard order since we just allocated it
                    let buf_slice = buf.as_slice_mut().unwrap();
                    let bytemuck_slice: &mut [$t] = bytemuck::cast_slice_mut(buf_slice);

                    match self.data_block.byte_order {
                        ByteOrder::Big => reader.$func::<BE>(bytemuck_slice),
                        ByteOrder::Little => reader.$func::<LE>(bytemuck_slice),
                    }.change_context(ReadDataBlockError::IoError)?;
                    Ok(buf.into_dyn_img(self.pixel_storage))
                }
            }
        }

        match self.sample_format {
            SampleFormat::UInt8 => self.read_data_impl(reader,
                Read::read_exact,
                Read::read_exact
            ).map(|buf| buf.into_dyn_img(self.pixel_storage)),
            SampleFormat::UInt16 => read_real!(read_u16_into),
            SampleFormat::UInt32 => read_real!(read_u32_into),
            SampleFormat::UInt64 => read_real!(read_u64_into),
            SampleFormat::Float32 => read_real!(read_f32_into),
            SampleFormat::Float64 => read_real!(read_f64_into),
            SampleFormat::Complex32 => read_complex!(read_f32_into, f32),
            SampleFormat::Complex64 => read_complex!(read_f64_into, f64),
        }
    }

    // TODO: handle out of memory errors gracefully instead of panicking, which I assume is the default behavior
    // F1 and F2 have identical signatures, but they need to be separate
    // because two functions with the same signature are not technically the same type according to rust
    fn read_data_impl<'a, T, F1, F2>(&self, reader: &'a mut dyn Read, read_le: F1, read_be: F2) -> Result<ArrayD<T>, ReadDataBlockError>
        where F1: Fn(&'a mut dyn Read, &mut [T]) -> std::io::Result<()>,
        F2: Fn(&'a mut dyn Read, &mut [T]) -> std::io::Result<()>,
        T: Clone + num_traits::Zero {
        let mut buf;
        match self.pixel_storage {
            PixelStorage::Planar => buf = ArrayD::<T>::zeros(IxDyn(&self.geometry[..])),
            PixelStorage::Normal => {
                let mut geometry = self.geometry.clone();
                geometry.rotate_left(1);
                buf = ArrayD::<T>::zeros(IxDyn(&geometry[..]));
            },
        }
        // this unwrap is safe because:
        // 1. all owned arrays are contiguous, and
        // 2. it's in standard order since we just allocated it
        let buf_slice = buf.as_slice_mut().unwrap();
        match self.data_block.byte_order {
            ByteOrder::Big => read_be(reader, buf_slice),
            ByteOrder::Little => read_le(reader, buf_slice),
        }.change_context(ReadDataBlockError::IoError)?;
        Ok(buf)
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
            .ok_or(report!(ReadPropertyError::NotFound))?;
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

    /// Returns true iff the given FITS key is present in the header
    pub fn has_fits_key(&self, name: impl AsRef<str>) -> bool {
        self.fits_header.get(name.as_ref()).is_some()
    }
    /// Attempts to parse a FITS key with the given name as type `T`, following the syntax laid out in
    /// [section 4.1](https://fits.gsfc.nasa.gov/standard40/fits_standard40aa-le.pdf#subsection.4.1) of the FITS specification.
    /// If there is more than one key present with the given name, only the first one is returned.
    pub fn parse_fits_key<T: FromFitsKey>(&self, name: impl AsRef<str>) -> Result<T, ReadFitsKeyError> {
        let content = self.fits_header.get(name.as_ref())
            .ok_or(report!(ReadFitsKeyError::NotFound))?;
        T::from_fits_key(content)
            .change_context(ReadFitsKeyError::InvalidFormat)
    }
    /// Returns an iterator over all values in the FITS header matching the given name.
    /// Each key is attempted to be parsed as type `T`, following the syntax laid out in
    /// [section 4.1](https://fits.gsfc.nasa.gov/standard40/fits_standard40aa-le.pdf#subsection.4.1) of the FITS specification.
    pub fn parse_fits_keys<T: FromFitsKey>(&self, name: impl AsRef<str>) -> impl Iterator<Item = Result<T, ParseValueError>> + '_ {
        self.fits_header.get_all(name.as_ref())
            .map(|content| T::from_fits_key(content))
    }
    /// Returns the raw string (both value and comment) of the FITS key matching the given name
    /// As of the time of writing, this is the only way to get comments from the FITS header
    pub fn raw_fits_key(&self, name: impl AsRef<str>) -> Option<&FitsKeyContent> {
        self.fits_header.get(name.as_ref())
    }
    /// Returns an iterator over all values (and comments) of keys in the FITS header matching the given name.
    /// Although most keys are only allowed to appear once in a header, this is especially useful for the HISTORY keyword,
    /// which is typically appended each time the image is processed in some way
    pub fn raw_fits_keys(&self, name: impl AsRef<str>) -> impl Iterator<Item = &FitsKeyContent> {
        self.fits_header.get_all(name.as_ref())
    }
    /// Iterates through all FITS keys as (key, value+comment) tuples,
    /// in the order they appear in file, returned as raw unparsed strings.
    pub fn all_raw_fits_keys(&self) -> impl Iterator<Item = (&String, &FitsKeyContent)> {
        self.fits_header.iter()
    }

    /// Returns a reference to the embedded ICC profile, if one exists.
    /// If the returned value is `Some`, obtain the profile data by calling `read_data()` on the contained value.
    /// Note: `read_data()` just returns a `Vec<u8>`; consider the `lcms2` crate if you need to actually decode it.
    pub fn icc_profile(&self) -> Option<&ICCProfile> {
        self.icc_profile.as_ref()
    }

    /// Returns a reference to the RGB working space, if one is specified.
    /// If none is specified, the default is the sRGB color space, relative to the D50 standard illuminant.
    /// Consider using [`Option::unwrap_or_default()`] on the result.
    pub fn rgb_working_space(&self) -> Option<&RGBWorkingSpace> {
        self.rgb_working_space.as_ref()
    }

    /// Returns a reference to the display function, if one is specified.
    /// If none is specified, the default is the identity function.
    /// Although the identity display function is the [`Default`], using [`Option::unwrap_or_default()`] on the result
    /// would likely be unwise for most cases, as applying the identity function would result in unnecessary computation.
    pub fn display_function(&self) -> Option<&DisplayFunction> {
        self.display_function.as_ref()
    }

    /// Returns the pixel density of this image, in pixels-per-inch or pixels-per-centimeter
    /// If none is specified, the default is 72 PPI.
    /// Consider using [`Option::unwrap_or_default()`] on the result
    pub fn pixel_density(&self) -> Option<&Resolution> {
        self.resolution.as_ref()
    }
}

///
///
/// XISF images may have an arbitrary number of dimensions and
#[derive(Clone, Debug)]
pub struct Image {
    base: ImageBase,
    bounds: Option<SampleBounds>,
    color_filter_array: Option<CFA>,
    thumbnail: Option<Thumbnail>,
}


fn parse_image<T: ParseImage + 'static>(node: RoNode, xpath: &XpathContext, opts: &ReadOptions) -> Result<Image, ParseNodeError> {
    let is_thumbnail = TypeId::of::<T>() == TypeId::of::<Thumbnail>();

    let context = |kind| -> ParseNodeError {
        ParseNodeError::new(T::TAG_NAME, kind)
    };
    let report = |kind| -> Report<ParseNodeError> {
        report!(ParseNodeError::new(T::TAG_NAME, kind))
    };

    // * this is mutable because we use .remove() instead of .get()
    // that way, after we've extracted everything we recognize,
    // we can just iterate through what remains and emit warnings
    // saying we don't know what so-and-so attribute means
    let mut attrs = node.get_attributes();

    let data_block = DataBlock::parse_node(node, T::TAG_NAME, &mut attrs)?
        .ok_or(context(MissingAttr))
        .attach_printable("Missing location attribute: Image elements must have a data block")?;

    let mut geometry: Vec<usize> = vec![];
    if let Some(dims) = attrs.remove("geometry") {
        for i in dims.split(":") {
            let dim = parse_auto_radix::<usize>(i.trim())
                .change_context(context(InvalidAttr))
                .attach_printable("Invalid geometry attribute: failed to parse dimension/channel count")
                .attach_printable_lazy(|| format!("Expected pattern \"{{dim_1}}:...:{{dim_N}}:{{channel_count}}\" (for N>=1 and all values > 0), found \"{i}\""))?;
            if dim > 0 {
                geometry.push(dim);
            } else {
                return Err(report(InvalidAttr))
                    .attach_printable("Invalid geometry attribute: dimensions and channel count all must be nonzero")
            }
        }
        if geometry.len() < 2 {
            return Err(report(InvalidAttr))
                .attach_printable("Invalid geometry attribute: must have at least one dimension and one channel")
        } else {
            // convert to row-major order
            geometry = geometry.into_iter().rev().collect();
        }
    } else {
        return Err(report(MissingAttr)).attach_printable("Missing geometry attribute")
    }

    let sample_format = attrs.remove("sampleFormat")
        .ok_or(report(MissingAttr))
        .attach_printable("Missing sampleFormat attribute")
        .and_then(|val| {
            val.parse::<SampleFormat>()
                .change_context(context(InvalidAttr))
                .attach_printable_lazy(||
                    format!("Invalid sampleFormat attribute: expected one of {:?}, found {val}", SampleFormat::VARIANTS))
        })?;

    let bounds = if let Some(val) = attrs.remove("bounds") {
        let (low, high) = val.split_once(":")
            .ok_or(report(InvalidAttr))
            .attach_printable_lazy(|| "Invalid bounds attribute: expected pattern \"low:high\", found \"{val}\"")?;

        Some(SampleBounds {
            low: low.trim().parse::<f64>()
                .change_context(context(InvalidAttr))
                .attach_printable("Invalid bounds attribute: failed to parse lower bound")?,
            high: high.trim().parse::<f64>()
                .change_context(context(InvalidAttr))
                .attach_printable("Invalid bounds attribute: failed to parse upper bound")?
        })
    } else if sample_format.requires_bounds() {
        return Err(report(MissingAttr))
            .attach_printable(format!("Missing bounds attribute: required when using using {sample_format} sample format"));
    } else {
        None
    };

    let image_type = if let Some(val) = attrs.remove("imageType") {
        Some(val.parse::<ImageType>()
            .change_context(context(InvalidAttr))
            .attach_printable_lazy(||
                format!("Invalid imageType attribute: expected one of {:?}, found {val}", ImageType::VARIANTS))?)
    } else {
        None
    };

    let pixel_storage = if let Some(val) = attrs.remove("pixelStorage") {
        val.parse::<PixelStorage>()
            .change_context(context(InvalidAttr))
            .attach_printable_lazy(||
                format!("Invalid pixelStorage attribute: expected one of {:?}, found {val}", PixelStorage::VARIANTS)
            )?
    } else {
        Default::default()
    };

    let color_space = if let Some(val) = attrs.remove("colorSpace") {
        val.parse::<ColorSpace>()
            .change_context(context(InvalidAttr))
            .attach_printable_lazy(||
                format!("Invalid colorSpace attribute: expected one of {:?}, found {val}", ColorSpace::VARIANTS)
            )?
    } else {
        Default::default()
    };

    let offset = if let Some(val) = attrs.remove("offset") {
        let maybe_negative = val.parse::<f64>()
            .change_context(context(InvalidAttr))
            .attach_printable("Invalid offset attribute")?;
        if maybe_negative < 0.0 {
            return Err(report!(context(InvalidAttr))).attach_printable("Invalid offset attribute: must be zero or greater")
        } else {
            maybe_negative
        }
    } else {
        0.0
    };

    let orientation = if let Some(val) = attrs.remove("orientation") {
        Some(val.parse::<Orientation>()
            .change_context(context(InvalidAttr))
            .attach_printable("Invalid orientation attribute")?)
    } else {
        None
    };

    let id = attrs.remove("id");
    if let Some(id) = &id {
        if !is_valid_id(id) {
            return Err(report(InvalidAttr)).attach_printable(
                format!("Invalid id attribute: must match regex [_a-zA-Z][_a-zA-Z0-9]*, found \"{id}\"")
            )
        }
    }

    let uuid = if let Some(val) = attrs.remove("uuid") {
        Some(val.parse::<Uuid>()
            .change_context(context(InvalidAttr))
            .attach_printable("Invalid uuid attribute")?)
    } else {
        None
    };

    for remaining in attrs.into_iter() {
        tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
    }

    let mut properties = HashMap::new();
    let mut fits_header = ListOrderedMultimap::new();
    let mut icc_profile = None;
    let mut rgb_working_space = None;
    let mut display_function = None;
    let mut color_filter_array = None;
    let mut resolution = None;
    let mut thumbnail = None;

    // TODO: ignore text/<Data> children of nodes with inline or embedded blocks, respectively
    for mut child in node.get_child_nodes() {
        child = child.follow_reference(xpath).change_context(context(InvalidReference))?;

        macro_rules! parse_optional {
            ($t:ty, $opt_out:ident) => {
                {
                    let parsed = <$t>::parse_node(child)?;
                    if $opt_out.replace(parsed).is_some() {
                        tracing::warn!(concat!("Duplicate ", stringify!($t), " element found -- discarding the previous one"));
                    }
                }
            };
            ($t:ty, $opt_out:ident, full) => {
                {
                    let parsed = <$t>::parse_node(child, xpath, opts)?;
                    if $opt_out.replace(parsed).is_some() {
                        tracing::warn!(concat!("Duplicate ", stringify!($t), " element found -- discarding the previous one"));
                    }
                }
            };
        }

        match child.get_name().as_str() {
            "Property" => {
                let prop = Property::parse_node(child)?;
                if properties.insert(prop.id.clone(), prop.content).is_some() {
                    tracing::warn!("Duplicate property found with id {} -- discarding the previous one", prop.id);
                }
            }
            "FITSKeyword" if opts.import_fits_keywords => {
                let key = FitsKeyword::parse_node(child)?;
                fits_header.append(key.name, key.content);
                // TODO: respect fits_keywords_as_properties option
            },
            "ICCProfile" => parse_optional!(ICCProfile, icc_profile),
            "RGBWorkingSpace" => parse_optional!(RGBWorkingSpace, rgb_working_space),
            "DisplayFunction" => parse_optional!(DisplayFunction, display_function),
            "ColorFilterArray" if !is_thumbnail => parse_optional!(CFA, color_filter_array),
            "Resolution" => parse_optional!(Resolution, resolution),
            "Thumbnail" if !is_thumbnail => parse_optional!(Thumbnail, thumbnail, full),
            bad => tracing::warn!("Ignoring unrecognized child node <{}>", bad),
        }
    }

    //===============//
    // SANITY CHECKS //
    //===============//

    if geometry[0] < color_space.num_channels() {
        return Err(report(InvalidAttr))
            .attach_printable(format!(
                "Insufficient color channels: {color_space} color space requires {}; only found {}",
                color_space.num_channels(),
                geometry[0]
            ));
    }

    if color_filter_array.is_some() && geometry.len() - 1 != 2 {
        tracing::warn!("ColorFilterArray element only has a defined meaning for 2D images; found one on a {}D image", geometry.len() - 1);
    }

    Ok(Image {
        base: ImageBase {
            data_block,
            geometry,
            sample_format,

            image_type,
            pixel_storage,
            color_space,
            offset,
            orientation,
            id,
            uuid,

            properties,
            fits_header,
            icc_profile,
            rgb_working_space,
            display_function,
            resolution,
        },
        bounds,
        color_filter_array,
        thumbnail,
    })
}

pub(crate) trait ParseImage: Sized {
    const TAG_NAME: &'static str;
}

impl ParseImage for Image {
    const TAG_NAME: &'static str = "Image";
}

impl Deref for Image {
    type Target = ImageBase;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl Image {
    pub(crate) fn parse_node(node: RoNode, xpath: &XpathContext, opts: &ReadOptions) -> Result<Self, ParseNodeError> {
        parse_image::<Self>(node, xpath, opts)
    }

    /// The minimum and maximum value for a pixel sample in this image.
    /// For images in a non-RGB [color space](ColorSpace), these bounds apply to pixel sample values once converted to RGB, not in its native color space.
    /// For integer [sample format](SampleFormat)s,
    pub fn bounds(&self) -> Option<SampleBounds> {
        self.bounds
    }

    /// Returns the color filter array, if one exists
    pub fn cfa(&self) -> Option<&CFA> {
        self.color_filter_array.as_ref()
    }

    /// Returns the thumbnail, if one exists
    pub fn thumbnail(&self) -> Option<&Thumbnail> {
        self.thumbnail.as_ref()
    }
}

/// Describes what kind of data makes up a given image
///
/// See also [`DynImageData`]
#[derive(Clone, Copy, Debug, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum SampleFormat {
    /// Pixel samples are scalar `u8`s
    UInt8,
    /// Pixel samples are scalar `u16`s
    UInt16,
    /// Pixel samples are scalar `u32`s
    UInt32,
    /// Pixel samples are scalar `u64`s
    UInt64,
    /// Pixel samples are scalar `f32`s
    Float32,
    /// Pixel samples are scalar `f64`s
    Float64,
    /// Pixel samples are complex with `f32` parts
    Complex32,
    /// Pixel samples are complex with `f64` parts
    Complex64,
}
impl SampleFormat {
    pub(crate) fn requires_bounds(&self) -> bool {
        match self {
            Self::Float32 | Self::Float64 => true,
            _ => false,
        }
    }
}

/// Sets the minimum and maximum value of a pixel sample
// TODO: I think this is just used for display purposes and doesn't actually clamp input? should add that to the doc if so
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SampleBounds {
    /// The lower bound
    pub low: f64,
    /// The upper bound
    pub high: f64,
}

/// Describes whether this is a light frame, dark frame, flat frame, bias frame, etc
#[derive(Clone, Copy, Debug, Display, EnumString, EnumVariantNames, PartialEq)]
pub enum ImageType {
    /// A bias frame is an extremely short exposure taken with the sensor covered,
    /// taken to counteract the sensor's readout noise.
    Bias,
    /// A dark frame is an exposure taken with the sensor covered, with the same length and at the same temperature as the light frame it's calibrating.
    /// Dark frames are taken to counteract the sensor's thermal noise and hot/cold pixels.
    Dark,
    /// A flat frame is an exposure taken with the main optical element completely illuminated by an even light source,
    /// taken to counteract the optical system's dust and vignetting.
    Flat,
    /// A light frame is an exposure of the actual target
    Light,
    /// An integration (AKA stack) of two or more [`Bias`](Self::Bias) frames
    MasterBias,
    /// An integration (AKA stack) of two or more [`Dark`](Self::Dark) frames
    MasterDark,
    /// An integration (AKA stack) of two or more [`Flat`](Self::Flat) frames
    MasterFlat,
    /// An integration (AKA stack) of two or more [`Light`](Self::Light) frames
    MasterLight,
    /// An integer or floating point real image where nonzero pixel sample values represent invalid or defective pixels.
    /// Defective pixels are typically ignored or replaced with plausible statistical estimates, such as robust averages of neighbor pixels.
    DefectMap,
    /// For a process that takes multiple images as an input and produces one output while rejecting outliers, such as integration.
    /// For any given pixel on the rejection map, the minimum value\* indicates that none of the corresponding pixels from the input images
    /// were rejected for being **high outliers**, and following a linear scale between, the maximum value\* indicates that all of the
    /// corresponding pixels from the input images were rejected. May be an integer or floating point real image.
    ///
    /// \* The minimum and maximum values of an [`Image`] are specified in the [`"bounds"`](SampleBounds) attribute of an [`Image`].
    /// Optional for integer images, where the default is the lower and upper bounds of the integer type.
    RejectionMapHigh,
    /// For a process that takes multiple images as an input and produces one output while rejecting outliers, such as integration.
    /// For any given pixel on the rejection map, the minimum value\* indicates that none of the corresponding pixels from the input images
    /// were rejected for being **low outliers**, and following a linear scale between, the maximum value\* indicates that all of the
    /// corresponding pixels from the input images were rejected. May be an integer or floating point real image.
    ///
    /// \* The minimum and maximum values of an [`Image`] are specified in the [`"bounds"`](SampleBounds) attribute of an [`Image`].
    /// Optional for integer images, where the default is the lower and upper bounds of the integer type.
    RejectionMapLow,
    /// For a process that takes a single image as an input and produces one output while rejecting outliers, such as cosmetic correction.
    /// For any given pixel on the rejection map, any nonzero value indicates that the corresponding input pixel was rejected for being a **high outlier**,
    /// and a value of 0 indicates that the pixel was not rejected. Must be an 8-bit unsigned integer image.
    BinaryRejectionMapHigh,
    /// For a process that takes a single image as an input and produces one output while rejecting outliers, such as cosmetic correction.
    /// For any given pixel on the rejection map, any nonzero value indicates that the corresponding input pixel was rejected for being a **low outlier**,
    /// and a value of 0 indicates that the pixel was not rejected. Must be an 8-bit unsigned integer image.
    BinaryRejectionMapLow,
    /// Integer or floating point real images where each pixel sample value is proportional to the slope of a straight line
    /// fitted to a set of integrated pixels at the corresponding pixel coordinates.
    /// A slope map value equal to the lower bound of the representable range corresponds to a
    /// horizontal line (or a slope of zero degrees), while the upper bound represents a vertical line (infinite slope).
    SlopeMap,
    /// Integer or floating point real images where each pixel sample value is proportional to a statistical weight assigned
    /// at the corresponding pixel coordinates. Statistical weights represented by weight maps are typically generated by image calibration,
    /// registration and integration processes, but can be produced by any task performing per-pixel evaluations or comparisons.
    WeightMap,
}

/// Describes the memory layout of the image's pixel samples
///
/// No matter which pixel storage layout is used, the pixel samples are stored in row-major order.
/// See [the specification](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#pixel_storage_models) for more details and a visual representation of each layout.
#[derive(Clone, Copy, Debug, Display, Default, EnumString, EnumVariantNames, PartialEq)]
pub enum PixelStorage {
    /// The image is stored as all of the first channel, then all of the second channel, and so on.
    /// That is, for a W\*H 2D image with 3 channels and `u8` samples, pixel *p<sub>x,y,c</sub>* is stored at byte offset *WHc + Wy + x*.
    /// Default when none is specified. See also [`memory_layout::Planar`].
    #[default]
    Planar,
    /// The image is stored as the first pixel (its first channel, second channel, and so on),
    /// the second pixel (its first channel, second channel, and so on), and so on.
    /// That is, for a W\*H 2D image with 3 channels and `u8` samples, pixel *p<sub>x,y,c</sub>* is stored at byte offset *3Wy + 3x + c*.
    /// See also [`memory_layout::Normal`].
    Normal,
}

/// An image's [color space](https://en.wikipedia.org/wiki/Color_space)
#[derive(Clone, Copy, Debug, Display, Default, EnumString, EnumVariantNames, PartialEq)]
pub enum ColorSpace {
    /// A one-channel grayscale image. Default when none is specified.
    #[default]
    Gray,
    /// A three-channel RGB image. Arguably only a color *model* and not a color *space*;
    /// requires context from an [`RGBWorkingSpace`] or [`ICCProfile`] element to become the latter.
    /// Should be considered coordinates in the sRGB color space (relative to the D50 standard illuminant) if neither supporting element is present.
    RGB,
    /// A three-channel CIE L\*a\*b\* image
    CIELab,
}
impl ColorSpace {
    /// Returns the number of channels that it takes to store a complete pixel in this color space
    pub fn num_channels(&self) -> usize {
        match self {
            Self::Gray => 1,
            Self::RGB | Self::CIELab => 3,
        }
    }
}

/// A transformation to be applied before *visual presentation* of the image
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Orientation {
    /// What kind of rotation should be applied, if any
    ///
    /// Should be applied *before* the horizontal flip, if a flip is present
    pub rotation: Rotation,
    /// True iff a horizontal reflection should be applied
    ///
    /// Should be applied *after* the rotation, if a rotation is present.
    /// *Reminder: a horizontal reflection of a 2D image flips pixels across the y axis,
    /// leaving pixels on the far-right of the image on the far-left and vice-versa*
    pub hflip: bool,
}
impl fmt::Display for Orientation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // if there is a horizontal flip but no rotation, it's serialized just as "flip", not "0;flip"
        if self.hflip && self.rotation == Rotation::None {
            f.write_str("flip")
        }
        else {
            f.write_fmt(format_args!("{}{}",
                self.rotation,
                if self.hflip { ";flip" } else { "" }
            ))
        }
    }
}
impl FromStr for Orientation {
    type Err = Report<ParseValueError>;
    fn from_str(s: &str) -> Result<Self, ParseValueError> {
        match s {
            "0" => Ok(Self { rotation: Rotation::None, hflip: false }),
            "flip" => Ok(Self { rotation: Rotation::None, hflip: true }),
            "90" => Ok(Self { rotation: Rotation::Ccw90, hflip: false }),
            "90;flip" => Ok(Self { rotation: Rotation::Ccw90, hflip: true }),
            "-90" => Ok(Self { rotation: Rotation::Cw90, hflip: false }),
            "-90;flip" => Ok(Self { rotation: Rotation::Cw90, hflip: true }),
            "180" => Ok(Self { rotation: Rotation::_180, hflip: false }),
            "180;flip" => Ok(Self { rotation: Rotation::_180, hflip: true }),
            bad => Err(report!(ParseValueError("Orientation")))
                .attach_printable(format!("Expected one of [0, flip, 90, 90;flip, -90, -90;flip, 180, 180;flip], found {bad}",))
        }
    }
}

/// A rotation (in multiples of 90 degrees) to be applied before visual presentation of the image
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub enum Rotation {
    /// No rotation
    #[default]
    None,
    /// A 90 degree clockwise rotation
    Cw90,
    /// A 90 degree counterclockwise rotation
    Ccw90,
    /// A 180 degree rotation
    _180,
}
impl fmt::Display for Rotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Rotation::None => "0",
            Rotation::Cw90 => "-90",
            Rotation::Ccw90 => "90",
            Rotation::_180 => "180",
        })
    }
}
