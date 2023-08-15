use std::io::Read;

use byteorder::{ReadBytesExt, LE, BE};
use error_stack::{IntoReport, Report, report, ResultExt};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};
use ndarray::{ArrayD, IxDyn};
use num_complex::Complex;
use parse_int::parse as parse_auto_radix;
use strum::{Display, EnumString, EnumVariantNames, VariantNames};
use uuid::Uuid;
use crate::{
    data_block::{DataBlock, ByteOrder},
    error::ReadDataBlockError,
    ReadOptions,
    ParseNodeError,
};

#[derive(Clone, Debug)]
pub struct Image {
    pub uid: Option<String>,

    pub data_block: DataBlock,
    pub geometry: Vec<usize>,
    pub sample_format: SampleFormat,

    // for images in a non-RGB color space, these bounds apply to pixel sample values once converted to RGB, not in its native color space
    pub bounds: Option<SampleBounds>,
    pub image_type: Option<ImageType>,
    pub pixel_storage: PixelStorage,
    pub color_space: ColorSpace,
    pub offset: f64,
    pub id: Option<String>,
    pub uuid: Option<Uuid>,
}

impl Image {
    pub(crate) fn parse_node(node: RoNode, xpath: &XpathContext, _opts: &ReadOptions) -> Result<Self, Report<ParseNodeError>> {
        const CONTEXT: ParseNodeError = ParseNodeError("Image");
        let _span_guard = tracing::debug_span!("<Image>").entered();

        // * this is mutable because we use .remove() instead of .get()
        // that way, after we've extracted everything we recognize,
        // we can just iterate through what remains and emit warnings
        // saying we don't know what so-and-so attribute means
        let mut attrs = node.get_attributes();

        let data_block = DataBlock::parse_node(node, xpath, CONTEXT, &mut attrs)?
            .ok_or(report!(CONTEXT))
            .attach_printable("Missing location attribute: <Image> nodes must have a data block")?;

        let mut geometry: Vec<usize> = vec![];
        if let Some(dims) = attrs.remove("geometry") {
            for i in dims.split(":") {
                let dim = parse_auto_radix::<usize>(i.trim())
                    .into_report()
                    .change_context(CONTEXT)
                    .attach_printable("Invalid geometry attribute: failed to parse dimension/channel count")
                    .attach_printable_lazy(|| format!("Expected pattern \"{{dim_1}}:...:{{dim_N}}:{{channel_count}}\" (for N>=1 and all values > 0), found \"{i}\""))?;
                if dim > 0 {
                    geometry.push(dim);
                } else {
                    return Err(report!(CONTEXT))
                        .attach_printable("Invalid geometry attribute: dimensions and channel count all must be nonzero")
                }
            }
            if geometry.len() < 2 {
                return Err(report!(CONTEXT))
                    .attach_printable("Invalid geometry attribute: must have at least one dimension and one channel")
            }
        } else {
            return Err(report!(CONTEXT)).attach_printable("Missing geometry attribute")
        }

        let sample_format = attrs.remove("sampleFormat")
            .ok_or(report!(CONTEXT))
            .attach_printable("Missing sampleFormat attribute")
            .and_then(|val| {
                val.parse::<SampleFormat>()
                    .into_report()
                    .change_context(CONTEXT)
                    .attach_printable_lazy(||
                        format!("Invalid sampleFormat attribute: expected one of {:?}, found {val}", SampleFormat::VARIANTS))
            })?;

        let bounds = if let Some(val) = attrs.remove("bounds") {
            let (low, high) = val.split_once(":")
                .ok_or(report!(CONTEXT))
                .attach_printable_lazy(|| "Invalid bounds attribute: expected pattern \"low:high\", found \"{val}\"")?;

            Some(SampleBounds {
                low: low.trim().parse::<f64>()
                    .into_report()
                    .change_context(CONTEXT)
                    .attach_printable("Invalid bounds attribute: failed to parse lower bound")?,
                high: high.trim().parse::<f64>()
                    .into_report()
                    .change_context(CONTEXT)
                    .attach_printable("Invalid bounds attribute: failed to parse upper bound")?
            })
        } else if sample_format.requires_bounds() {
            return Err(report!(CONTEXT))
                .attach_printable(format!("Missing bounds attribute: required when using using {sample_format} sample format"));
        } else {
            None
        };

        let image_type = if let Some(val) = attrs.remove("imageType") {
            Some(val.parse::<ImageType>()
                .into_report()
                .change_context(CONTEXT)
                .attach_printable_lazy(||
                    format!("Invalid imageType attribute: expected one of {:?}, found {val}", ImageType::VARIANTS))?)
        } else {
            None
        };

        let pixel_storage = if let Some(val) = attrs.remove("pixelStorage") {
            val.parse::<PixelStorage>()
                .into_report()
                .change_context(CONTEXT)
                .attach_printable_lazy(||
                    format!("Invalid pixelStorage attribute: expected one of {:?}, found {val}", PixelStorage::VARIANTS)
                )?
        } else {
            Default::default()
        };

        for remaining in attrs.into_iter() {
            tracing::warn!("Ignoring unrecognized attribute {}=\"{}\"", remaining.0, remaining.1);
        }

        // TODO: ignore text/<Data> children of nodes with inline or embedded blocks, respectively
        for child in node.get_child_nodes() {
            tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
        }

        Ok(Image {
            uid: None,

            data_block,
            geometry: geometry.into_iter().rev().collect(), // swap to row-major order
            sample_format,

            bounds,
            image_type,
            pixel_storage,
            // TODO: parse these attributes
            color_space: Default::default(),
            offset: 0.0,
            id: None,
            uuid: None,
        })
    }

    // cull leading 1-size dimensions, useful for compatibility with FITS
    // some FITS readers incorrectly use NAXIS=2 vs 3 as an indicator of a grayscale vs RGB image
    pub fn geometry_trimmed(&self) -> &[usize] {
        let mut trim = 0;
        for i in &self.geometry {
            if *i == 1 {
                trim += 1;
            }
        }
        &self.geometry[trim..]
    }
    pub fn num_dimensions(&self) -> usize {
        self.geometry.len() - 1
    }
    pub fn num_channels(&self) -> usize {
        self.geometry[0]
    }

    // TODO: convert CIE L*a*b images to RGB
    pub fn read_data(&self, root: &crate::XISF) -> Result<ImageData, Report<ReadDataBlockError>> {
        let mut reader = self.data_block.location.bytes(&root)?;
        match self.sample_format {
            SampleFormat::UInt8 => Ok(ImageData::UInt8(
                self.read_data_impl(&mut reader,
                    Box::<dyn Read>::read_exact,
                    Box::<dyn Read>::read_exact
                )?
            )),
            SampleFormat::UInt16 => Ok(ImageData::UInt16(
                self.read_data_impl(&mut reader,
                    Box::<dyn Read>::read_u16_into::<LE>,
                    Box::<dyn Read>::read_u16_into::<BE>
                )?
            )),
            SampleFormat::UInt32 => Ok(ImageData::UInt32(
                self.read_data_impl(&mut reader,
                    Box::<dyn Read>::read_u32_into::<LE>,
                    Box::<dyn Read>::read_u32_into::<BE>
                )?
            )),
            SampleFormat::UInt64 => Ok(ImageData::UInt64(
                self.read_data_impl(&mut reader,
                    Box::<dyn Read>::read_u64_into::<LE>,
                    Box::<dyn Read>::read_u64_into::<BE>
                )?
            )),
            SampleFormat::Float32 => Ok(ImageData::Float32(
                self.read_data_impl(&mut reader,
                    Box::<dyn Read>::read_f32_into::<LE>,
                    Box::<dyn Read>::read_f32_into::<BE>
                )?
            )),
            SampleFormat::Float64 => Ok(ImageData::Float64(
                self.read_data_impl(&mut reader,
                    Box::<dyn Read>::read_f64_into::<LE>,
                    Box::<dyn Read>::read_f64_into::<BE>
                )?
            )),
            SampleFormat::Complex32 => {
                // TODO read complex images
                Err(report!(ReadDataBlockError))
                    .attach_printable("Reading complex-valued images is not yet supported")
            }
            SampleFormat::Complex64 => {
                // TODO read complex images
                Err(report!(ReadDataBlockError))
                    .attach_printable("Reading complex-valued images is not yet supported")
            }
        }
    }

    // TODO: extract out some of this to DataBlock for re-use with vector and matrix blocks
    // TODO: do I actually need to zero out that memory? `ArrayBase::uninit()` looks like a tempting way to save cycles
    // TODO: handle out of memory errors gracefully instead of panicking, which I assume is the default behavior
    // F1 and F2 have identical signatures, but they need to be separate
    // because two functions with the same signature are not technically the same type according to rust
    fn read_data_impl<T, F1, F2>(&self, reader: &mut Box<dyn Read>, read_le: F1, read_be: F2) -> Result<ArrayD<T>, Report<ReadDataBlockError>>
        where F1: Fn(&mut Box<dyn Read>, &mut [T]) -> std::io::Result<()>,
        F2: Fn(&mut Box<dyn Read>, &mut [T]) -> std::io::Result<()>,
        T: Clone + num_traits::Zero {
        let mut buf;
        match self.pixel_storage {
            PixelStorage::Planar => {
                buf = ArrayD::<T>::zeros(IxDyn(&self.geometry[..]));
                let buf_slice = buf.as_slice_mut()
                    .ok_or(report!(ReadDataBlockError))
                    .attach_printable("Failed to get write access to output buffer")?;
                match self.data_block.byte_order {
                    ByteOrder::Big => read_be(reader, buf_slice),
                    ByteOrder::Little => read_le(reader, buf_slice),
                }.into_report().change_context(ReadDataBlockError)?;
            }
            PixelStorage::Normal => {
                let mut geometry = self.geometry.clone();
                geometry.rotate_left(1);
                buf = ArrayD::<T>::zeros(IxDyn(&geometry[..]));
                let buf_slice = buf.as_slice_mut()
                    .ok_or(report!(ReadDataBlockError))
                    .attach_printable("Failed to get write access to output buffer")?;
                match self.data_block.byte_order {
                    ByteOrder::Big => read_be(reader, buf_slice),
                    ByteOrder::Little => read_le(reader, buf_slice),
                }.into_report().change_context(ReadDataBlockError)?;
                // move the channel axis to the beginning instead of the end
                let mut axes: Vec<_> = (0..self.geometry.len()).into_iter().collect();
                axes.rotate_right(1);
                buf = buf.permuted_axes(axes.as_slice());
                // and apply the transformation to the memory layout
                buf = buf.as_standard_layout().to_owned();
            }
        }
        Ok(buf)
    }
}

#[derive(Clone, Copy, Debug, Display, EnumString, EnumVariantNames)]
pub enum SampleFormat {
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Complex32,
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

#[derive(Clone, Copy, Debug)]
pub struct SampleBounds {
    pub low: f64,
    pub high: f64,
}

#[derive(Clone, Debug)]
pub enum ImageData {
    UInt8(ArrayD<u8>),
    UInt16(ArrayD<u16>),
    UInt32(ArrayD<u32>),
    UInt64(ArrayD<u64>),
    Float32(ArrayD<f32>),
    Float64(ArrayD<f64>),
    Complex32(ArrayD<Complex<f32>>),
    Complex64(ArrayD<Complex<f64>>),
}

#[derive(Clone, Copy, Debug, Display, EnumString, EnumVariantNames)]
pub enum ImageType {
    Bias,
    Dark,
    Flat,
    Light,
    MasterBias,
    MasterDark,
    MasterFlat,
    MasterLight,
    DefectMap,
    RejectionMapHigh,
    RejectionMapLow,
    BinaryRejectionMapHigh,
    BinaryRejectionMapLow,
    SlopeMap,
    WeightMap,
}

#[derive(Clone, Copy, Debug, Display, Default, EnumString, EnumVariantNames)]
pub enum PixelStorage {
    #[default]
    Planar, // channels are contiguous in memory
    Normal, // pixels are contiguous in memory
}

#[derive(Clone, Copy, Debug, Display, Default, EnumString, EnumVariantNames)]
pub enum ColorSpace {
    #[default]
    Gray,
    RGB,
    CIELab,
}