use std::num::NonZeroU64;

use error_stack::{IntoReport, Report, report, ResultExt};
use libxml::readonly::RoNode;
use ndarray::ArrayD;
use num_complex::Complex;
use strum::{Display, EnumString, EnumVariantNames, VariantNames};
use crate::{
    data_block::{DataBlockLocation, Encoding},
    error::ReadImageError,
    ReadOptions,
    ParseNodeError,
};

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
pub enum ImageBounds {
    UInt8(u8, u8),
    UInt16(u16, u16),
    UInt32(u32, u32),
    UInt64(u64, u64),
    Float32(f32, f32),
    Float64(f64, f64),
    Complex32(Complex<f32>, Complex<f32>),
    Complex64(Complex<f64>, Complex<f64>),
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

// TODO: should image actually be an enum, just pushing all the SampleValue stuff to the top?
// Bit too big of a refactor before a commit, but something promising to play around with
pub struct Image {
    pub data_block: DataBlockLocation,
    pub geometry: Vec<NonZeroU64>,
    pub sample_format: SampleFormat,

    // for images in a non-RGB color space, these bounds apply to pixel sample values once converted to RGB, not in its native color space
    pub bounds: Option<ImageBounds>,
    pub image_type: Option<ImageType>,
}

impl Image {
    pub fn new(node: &RoNode, _opts: &ReadOptions) -> Result<Self, Report<ParseNodeError>> {
        const CONTEXT: ParseNodeError = ParseNodeError("Image");
        let _span_guard = tracing::debug_span!("<Image>").entered();

        // * this is mutable because we use .remove() instead of .get()
        // that way, after we've extracted everything we recognize,
        // we can just iterate through what remains and emit warnings
        // saying we don't know what so-and-so attribute means
        let mut attrs = node.get_attributes();

        let mut geometry: Vec<NonZeroU64> = vec![];
        if let Some(dims) = attrs.remove("geometry") {
            for i in dims.split(":") {
                geometry.push(i.parse::<NonZeroU64>()
                    .into_report()
                    .change_context(CONTEXT)
                    .attach_printable("Failed to parse geometry attribute")
                    .attach_printable_lazy(|| format!("Expected pattern \"{{dim_1}}:...:{{dim_N}}:{{channel_count}}\" (for N>=1 and all values > 0), found \"{i}\""))?);
            }
            if geometry.len() < 2 {
                return Err(report!(CONTEXT))
                    .attach_printable("Invalid geometry attribute: must have at least one dimension and one channel")
            }
        } else {
            return Err(report!(CONTEXT))
                .attach_printable("Missing geometry attribute")
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

        // let bounds = if let Some(val) = attrs.remove("bounds") {
        //     Some(val.split_once(":")
        //         .ok_or(report!(CONTEXT))
        //         .attach_printable_lazy(|| "Invalid bounds attribute: expected pattern \"low:high\", found \"{val}\"")
        //         .and_then(|(low, high)| {
        //             (
        //                 low.parse::<T>()
        //                     .into_report()
        //                     .change_context(CONTEXT)
        //                     .attach_printable("Invalid bounds attribute: failed to parse lower bound"),
        //                 high.parse::<T>()
        //                     .into_report()
        //                     .change_context(CONTEXT)
        //                     .attach_printable("Invalid bounds attribute: failed to parse upper bound")
        //             )
        //         })?)
        // } else if sample_format.requires_bounds() {
        //     return Err(report!(CONTEXT))
        //         .attach_printable(format!(""));
        // } else {
        //     None
        // };

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

        for child in node.get_child_nodes() {
            tracing::warn!("Ignoring unrecognized child node <{}>", child.get_name());
        }

        Ok(Image {
            data_block: DataBlockLocation::InlineOrEmbedded { encoding: Encoding::Base64, text: "FISHFASFHASJFKF".to_string() },
            geometry,
            sample_format,

            bounds: None,
            image_type,
        })
    }

    pub fn read_data(&mut self) -> Result<ImageData, Report<ReadImageError>> {
        todo!()
    }
    pub async fn read_data_async(&mut self) -> Result<ImageData, Report<ReadImageError>> {
        todo!()
    }
}