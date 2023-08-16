use std::{io::{Read, Write, Cursor}, fmt, str::FromStr};

use byteorder::{ReadBytesExt, LE, BE};
use digest::Digest;
use error_stack::{IntoReport, Report, report, ResultExt};
use libxml::{readonly::RoNode, xpath::Context as XpathContext};
use ndarray::{ArrayD, IxDyn};
use num_complex::Complex;
use parse_int::parse as parse_auto_radix;
use sha1::Sha1;
use sha2::{Sha256, Sha512};
use sha3::{Sha3_256, Sha3_512};
use strum::{Display, EnumString, EnumVariantNames, VariantNames};
use uuid::Uuid;
use crate::{
    data_block::{DataBlock, ByteOrder, Checksum},
    error::{ReadDataBlockError, ParseValueError},
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
    pub orientation: Orientation,
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

        let color_space = if let Some(val) = attrs.remove("colorSpace") {
            val.parse::<ColorSpace>()
                .into_report()
                .change_context(CONTEXT)
                .attach_printable_lazy(||
                    format!("Invalid colorSpace attribute: expected one of {:?}, found {val}", ColorSpace::VARIANTS)
                )?
        } else {
            Default::default()
        };

        let offset = if let Some(val) = attrs.remove("offset") {
            let maybe_negative = val.parse::<f64>()
                .into_report()
                .change_context(CONTEXT)
                .attach_printable("Invalid offset attribute")?;
            if maybe_negative < 0.0 {
                return Err(report!(CONTEXT)).attach_printable("Invalid offset attribute: must be zero or greater")
            } else {
                maybe_negative
            }
        } else {
            0.0
        };

        let orientation = if let Some(val) = attrs.remove("orientation") {
            val.parse::<Orientation>()
                .change_context(CONTEXT)
                .attach_printable("Invalid orientation attribute")?
        } else {
            Default::default()
        };

        let uid = attrs.remove("uid");
        if let Some(uid) = &uid {
            if !uid.chars().all(|c| c.is_alphanumeric()) {
                return Err(report!(CONTEXT)).attach_printable(
                    format!("Invalid uid attribute: expected all characters to be alphanumeric, found \"{uid}\"")
                )
            }
            // TODO: verify uid uniqueness
        }

        let id = attrs.remove("id");
        if let Some(id) = &id {
            if !id.chars().all(|c| c.is_alphanumeric()) {
                return Err(report!(CONTEXT)).attach_printable(
                    format!("Invalid id attribute: expected all characters to be alphanumeric, found \"{id}\"")
                )
            }
        }

        let uuid = if let Some(val) = attrs.remove("uuid") {
            Some(val.parse::<Uuid>()
                .into_report()
                .change_context(CONTEXT)
                .attach_printable("Invalid uuid attribute")?)
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
            uid,

            data_block,
            geometry: geometry.into_iter().rev().collect(), // swap to row-major order
            sample_format,

            bounds,
            image_type,
            pixel_storage,
            color_space,
            offset,
            orientation,
            id,
            uuid,
        })
    }

    pub fn num_dimensions(&self) -> usize {
        self.geometry.len() - 1
    }
    pub fn num_channels(&self) -> usize {
        self.geometry[0]
    }

    // TODO: convert CIE L*a*b images to RGB
    // ! output array will not be stored contiguously in memory if the pixel storage mode is Normal
    // ! if you need contiguous data, i.e. for interoperability, call `.as_standard_layout()` on the array before accessing the slice
    // ! be warned that `.as_standard_layout()` clones the entire memory block if it isn't a no-op
    pub fn read_data(&self, root: &crate::XISF) -> Result<ImageData, Report<ReadDataBlockError>> {
        fn verify_hash<D: Digest + Write>(expected: &[u8], reader: &mut dyn Read) -> Result<(), Report<ReadDataBlockError>> {
            let mut hasher = D::new();
            std::io::copy(reader, &mut hasher)
                .into_report()
                .change_context(ReadDataBlockError)
                .attach_printable("Failed to calculate image hash")?;
            let actual = hasher.finalize();
            if actual.as_slice() == expected {
                Ok(())
            } else {
                let actual = hex_simd::encode_to_string(actual.as_slice(), hex_simd::AsciiCase::Lower);
                let expected = hex_simd::encode_to_string(expected, hex_simd::AsciiCase::Lower);
                Err(report!(ReadDataBlockError))
                    .attach_printable(format!("Data block failed checksum verification: expected {expected}, found {actual}"))
            }
        }

        // read through the block twice, once to verify the checksum on the compressed bytes, and then again to read the decompressed bytes
        let mut reader = self.data_block.location.raw_bytes(&root)?;
        if let Some(checksum) = &self.data_block.checksum {
            match checksum {
                Checksum::Sha1(digest) => verify_hash::<Sha1>(digest, &mut reader)?,
                Checksum::Sha256(digest) => verify_hash::<Sha256>(digest, &mut reader)?,
                Checksum::Sha512(digest) => verify_hash::<Sha512>(digest, &mut reader)?,
                Checksum::Sha3_256(digest) => verify_hash::<Sha3_256>(digest, &mut reader)?,
                Checksum::Sha3_512(digest) => verify_hash::<Sha3_512>(digest, &mut reader)?,
            }
            reader = self.data_block.location.decompressed_bytes(&root, &self.data_block.compression)?;
        }

        match &self.data_block.compression {
            Some(compression) if compression.is_shuffled() => {
                let item_size = compression.shuffle_item_size().unwrap();
                // byte shuffling is a nop for 1 or 0 size items
                // not sure why any implementation would encode it like this, but best to save the clone I guess
                if item_size > 1 {
                    let n = compression.uncompressed_size() / item_size;
                    if n * item_size != compression.uncompressed_size() {
                        return Err(report!(ReadDataBlockError)).attach_printable("Uncompressed size is not divisible by item size")
                    }
                    // to unshuffle, call this same code block &[n, item_size] instead of &[item_size, n]
                    let mut buf = ArrayD::<u8>::zeros(IxDyn(&[n, item_size]));
                    reader.read_exact(buf.as_slice_memory_order_mut().unwrap())
                        .into_report()
                        .change_context(ReadDataBlockError)
                        .attach_printable("Failed to read bytes into temporary buffer for unshuffling")?;
                    buf.swap_axes(0, 1);
                    reader = Box::new(Cursor::new(buf.as_standard_layout().to_owned().into_raw_vec()));
                }
            }
            _ => (),
        }

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
            // TODO read complex images
            SampleFormat::Complex32 => {
                Err(report!(ReadDataBlockError))
                    .attach_printable("Reading complex-valued images is not yet supported")
            }
            SampleFormat::Complex64 => {
                Err(report!(ReadDataBlockError))
                    .attach_printable("Reading complex-valued images is not yet supported")
            }
        }
    }

    // TODO: extract out some of this to DataBlock for re-use with vector and matrix blocks
    // TODO: handle out of memory errors gracefully instead of panicking, which I assume is the default behavior
    // TODO: read normal pixel storage and byte-shuffled compression without duplicating the whole image
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
                // ! this is not reflected in the memory layout of the array
                let mut axes: Vec<_> = (0..self.geometry.len()).into_iter().collect();
                axes.rotate_right(1);
                buf = buf.permuted_axes(axes.as_slice());
            }
        }
        Ok(buf)
    }
}

#[derive(Clone, Copy, Debug, Display, EnumString, EnumVariantNames, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Clone, Copy, Debug, Display, EnumString, EnumVariantNames, PartialEq)]
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

#[derive(Clone, Copy, Debug, Display, Default, EnumString, EnumVariantNames, PartialEq)]
pub enum PixelStorage {
    #[default]
    Planar, // channels are contiguous in memory
    Normal, // pixels are contiguous in memory
}

#[derive(Clone, Copy, Debug, Display, Default, EnumString, EnumVariantNames, PartialEq)]
pub enum ColorSpace {
    #[default]
    Gray,
    RGB,
    CIELab,
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Orientation {
    pub rotation: Rotation,
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
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(Self { rotation: Rotation::None, hflip: false }),
            "flip" => Ok(Self { rotation: Rotation::None, hflip: true }),
            "90" => Ok(Self { rotation: Rotation::None, hflip: false }),
            "90;flip" => Ok(Self { rotation: Rotation::None, hflip: true }),
            "-90" => Ok(Self { rotation: Rotation::None, hflip: false }),
            "-90;flip" => Ok(Self { rotation: Rotation::None, hflip: true }),
            "180" => Ok(Self { rotation: Rotation::None, hflip: false }),
            "180;flip" => Ok(Self { rotation: Rotation::None, hflip: true }),
            bad => Err(report!(ParseValueError("Orientation")))
                .attach_printable(format!("Expected one of [0, flip, 90, 90;flip, -90, -90;flip, 180, 180;flip], found {bad}",))
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub enum Rotation {
    #[default]
    None,
    Cw90,
    Ccw90,
    _180,
}
impl Rotation {
    pub fn degrees(&self) -> i16 {
        match self {
            Rotation::None => 0,
            Rotation::Cw90 => -90,
            Rotation::Ccw90 => 90,
            Rotation::_180 => 180,
        }
    }
}
impl fmt::Display for Rotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.degrees()))
    }
}