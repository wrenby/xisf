use error_stack::{fmt::{Charset, ColorMode}, Report};
use fitsio::images::{ImageDescription, ImageType, WriteImage};
use ndarray::ArrayD;
use tracing::Level;
use tracing_error::ErrorLayer;
use tracing_subscriber::{prelude::*, FmtSubscriber};
use xisf::image::ImageData;
use std::{env, any::TypeId};

fn main() {
    let supports_color = supports_color::on_cached(supports_color::Stream::Stderr)
        .map_or(false, |level| level.has_basic);
    Report::set_color_mode(if supports_color { ColorMode::Color } else { ColorMode::None });

    let supports_unicode = supports_unicode::on(supports_unicode::Stream::Stderr);
    Report::set_charset(if supports_unicode { Charset::Utf8 } else { Charset::Ascii });

    // ! all errors resulting from invalid logging configuration are considered nonfatal
    // ! they get logged to stderr but the show goes on

    // emulate env_logger
    let level = match env::var("RUST_LOG")
        .map(|env| env.to_lowercase())
        .as_deref() {
        Ok("error") => Some(Level::ERROR),
        Ok("warn") => Some(Level::WARN),
        Ok("info") => Some(Level::INFO),
        Ok("debug") => Some(Level::DEBUG),
        Ok("trace") => Some(Level::TRACE),
        Ok(_) | Err(env::VarError::NotUnicode(_)) => {
            eprintln!("Invalid setting for environment variable RUST_LOG: expected one of [error, warn, info, debug, trace]");
            None
        },
        Err(env::VarError::NotPresent) => None,
    };
    if let Some(level) = level {
        let subscriber = FmtSubscriber::builder()
            .with_max_level(level)
            .with_writer(std::io::stderr)
            .finish()
            .with(ErrorLayer::default());

        if let Err(e) = tracing::subscriber::set_global_default(subscriber) {
            eprintln!("Failed to set subscriber for tracing logs: {}", e);
        }
    }

    let xisf = xisf::XISF::read_file("test-files/BiasStacked/test.xisf", &Default::default()).expect("parse header");
    println!("{xisf:#?}");

    let ndarray = xisf.images[0].read_data(&xisf).expect("read image data");
    println!("{:?}", ndarray);

    // TODO: support writing multiple images
    // ? how to make the first write to the primary instead of having an empty primary and all extensions?
    // - consider wrapping FitsFile in an iterator wrapper with Output = FitsHdu, and zipping it with an iterator over images
    fn write_array<T: WriteImage + Clone + 'static>(arr: &ArrayD<T>, name: impl AsRef<str>, format: ImageType) {
        // remove leading 1-size dimensions -- necessary for most FITS readers to identify grayscale images
        let mut trim = 0;
        for i in arr.shape() {
            if *i == 1 {
                trim += 1;
            }
        }
        let image_description = ImageDescription {
            data_type: format,
            dimensions: &arr.shape()[trim..],
        };

        let mut fits = fitsio::FitsFile::create("test-files/BiasStacked/converted.fits")
            .overwrite()
            .with_custom_primary(&image_description)
            .open()
            .expect("create fits file");

        // let hdu = fits.create_image("IMAGE_NAME", &image_description).expect("image extension hdu");
        let hdu = fits.primary_hdu().expect("primary hdu");

        if TypeId::of::<T>() == TypeId::of::<u64>() && format == ImageType::LongLong {
            // FITS doesn't exactly have a concept of unsigned data in data blocks
            // it's all stored as signed and stored with an offset key in the header to make up the difference
            // cfitsio has helpers to write u16 and u32 images that are exposed to the rust wrapper,
            // but we need to implement this feature manually for u64
            hdu.write_key(&mut fits, "BZERO", u64::MAX / 2).expect("write offset to header");
        }
        hdu.write_key(&mut fits, "EXTNAME", name.as_ref()).expect("write name to header");
        hdu.write_image(&mut fits, arr.as_standard_layout().as_slice_memory_order().unwrap())
            .expect("write image data");
    }

    let name = xisf.images[0].id.as_deref().unwrap_or("NAME");
    match ndarray {
        ImageData::UInt8(arr) => write_array(&arr, name, ImageType::UnsignedByte),
        ImageData::UInt16(arr) => write_array(&arr, name, ImageType::UnsignedShort),
        ImageData::UInt32(arr) => write_array(&arr, name, ImageType::UnsignedLong),
        ImageData::UInt64(arr) => write_array(&arr, name, ImageType::LongLong),
        ImageData::Float32(arr) => write_array(&arr, name, ImageType::Float),
        ImageData::Float64(arr) => write_array(&arr, name, ImageType::Double),
        ImageData::Complex32(_) | ImageData::Complex64(_) => eprintln!("FITS does not support complex images"),
    }

}