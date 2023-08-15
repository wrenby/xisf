use error_stack::{IntoReport, Report, report, ResultExt};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};
use ndarray::{ArrayD, IxDyn};
use num_complex::Complex;
use parse_int::parse as parse_auto_radix;
use strum::{Display, EnumString, EnumVariantNames, VariantNames};
use uuid::Uuid;
use crate::{
    data_block::DataBlock,
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
            geometry,
            sample_format,

            bounds,
            image_type,
            // TODO: parse these attributes
            pixel_storage: Default::default(),
            color_space: Default::default(),
            offset: 0.0,
            id: None,
            uuid: None,
        })
    }

    pub fn num_dimensions(&self) -> usize {
        self.geometry.len() - 1
    }
    pub fn num_channels(&self) -> usize {
        self.geometry[self.geometry.len() - 1]
    }

    // get the compiler to pipe down while I'm just trying to hack something together
    #[allow(unused_mut)]
    pub fn read_data(&self) -> Result<ImageData, Report<ReadDataBlockError>> {
        let total_len = self.geometry.iter().fold(1, |acc, n| acc * n);
        match self.sample_format {
            SampleFormat::UInt8 => {
                let mut buf = ArrayD::<u8>::zeros(IxDyn(&[total_len]));
                // TODO: fill buffer
                Ok(ImageData::UInt8(
                    buf.into_shape(self.geometry.as_slice())
                        .into_report()
                        .change_context(ReadDataBlockError)
                        .attach_printable("Failed to shape ndarray buffer to fit requested image dimensions")?
                ))
            }
            SampleFormat::UInt16 => {
                let mut buf = ArrayD::<u16>::zeros(IxDyn(&[total_len]));
                // TODO: fill buffer
                Ok(ImageData::UInt16(
                    buf.into_shape(self.geometry.as_slice())
                        .into_report()
                        .change_context(ReadDataBlockError)
                        .attach_printable("Failed to shape ndarray buffer to fit requested image dimensions")?
                ))
            }
            SampleFormat::UInt32 => {
                let mut buf = ArrayD::<u32>::zeros(IxDyn(&[total_len]));
                // TODO: fill buffer
                Ok(ImageData::UInt32(
                    buf.into_shape(self.geometry.as_slice())
                        .into_report()
                        .change_context(ReadDataBlockError)
                        .attach_printable("Failed to shape ndarray buffer to fit requested image dimensions")?
                ))
            }
            SampleFormat::UInt64 => {
                let mut buf = ArrayD::<u64>::zeros(IxDyn(&[total_len]));
                // TODO: fill buffer
                Ok(ImageData::UInt64(
                    buf.into_shape(self.geometry.as_slice())
                        .into_report()
                        .change_context(ReadDataBlockError)
                        .attach_printable("Failed to shape ndarray buffer to fit requested image dimensions")?
                ))
            }
            SampleFormat::Float32 => {
                let mut buf = ArrayD::<f32>::zeros(IxDyn(&[total_len]));
                // TODO: fill buffer
                Ok(ImageData::Float32(
                    buf.into_shape(self.geometry.as_slice())
                        .into_report()
                        .change_context(ReadDataBlockError)
                        .attach_printable("Failed to shape ndarray buffer to fit requested image dimensions")?
                ))
            }
            SampleFormat::Float64 => {
                let mut buf = ArrayD::<f64>::zeros(IxDyn(&[total_len]));
                // TODO: fill buffer
                Ok(ImageData::Float64(
                    buf.into_shape(self.geometry.as_slice())
                        .into_report()
                        .change_context(ReadDataBlockError)
                        .attach_printable("Failed to shape ndarray buffer to fit requested image dimensions")?
                ))
            }
            SampleFormat::Complex32 => {
                let mut buf = ArrayD::<Complex<f32>>::zeros(IxDyn(&[total_len]));
                // TODO: fill buffer
                Ok(ImageData::Complex32(
                    buf.into_shape(self.geometry.as_slice())
                        .into_report()
                        .change_context(ReadDataBlockError)
                        .attach_printable("Failed to shape ndarray buffer to fit requested image dimensions")?
                ))
            }
            SampleFormat::Complex64 => {
                let mut buf = ArrayD::<Complex<f64>>::zeros(IxDyn(&[total_len]));
                // TODO: fill buffer
                Ok(ImageData::Complex64(
                    buf.into_shape(self.geometry.as_slice())
                        .into_report()
                        .change_context(ReadDataBlockError)
                        .attach_printable("Failed to shape ndarray buffer to fit requested image dimensions")?
                ))
            }
        }
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

#[derive(Clone, Copy, Debug, Display, Default, EnumString)]
pub enum PixelStorage {
    #[default]
    Planar,
    Normal
}

#[derive(Clone, Copy, Debug, Display, Default, EnumString)]
pub enum ColorSpace {
    #[default]
    Grayscale,
    RGB,
    CIELab,
}