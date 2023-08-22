use error_stack::{fmt::{Charset, ColorMode}, Report};
use fitsio::{images::{ImageDescription, ImageType, WriteImage}, FitsFile};
use ndarray::ArrayD;
use tracing::Level;
use tracing_error::ErrorLayer;
use tracing_subscriber::{prelude::*, FmtSubscriber};
use xisf_rs::image::ImageData;
use std::{env, any::TypeId, fmt::Debug};

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

    let mut args = pico_args::Arguments::from_env();
    let input: String = args.value_from_str("--in").expect("--in");

    let xisf = xisf_rs::XISF::read_file(input, &Default::default()).expect("parse header");
    println!("{xisf:#?}");

    // passing the file as a &mut Option because we want to do something different the first time this is run
    // several FITS readers expect the primary hdu to contain image information in files with multiple images,
    // so we need a mechanism to create a primary hdu (that is, open the file itself) on demand to fit image specifications
    fn write_array<T: WriteImage + Clone + Debug + 'static>(fits: &mut Option<FitsFile>, out_name: &str, arr: &ArrayD<T>, name: impl AsRef<str>, format: ImageType) {
        println!("{:?}", arr);
        // remove channel as an axis if there's only one
        let trim = if arr.shape().starts_with(&[1]) { 1 } else { 0 };
        let image_description = ImageDescription {
            data_type: format,
            dimensions: &arr.shape()[trim..],
        };

        let first_image = fits.is_none();
        let fits = fits.get_or_insert_with(|| {
            fitsio::FitsFile::create(out_name)
                .overwrite()
                .with_custom_primary(&image_description)
                .open()
                .expect("create fits file")
        });
        // create_image adds an EXTNAME key automatically, but primary HDUs don't have one by default
        let hdu = if first_image {
            let hdu = fits.primary_hdu().expect("primary hdu");
            hdu.write_key(fits, "EXTNAME", name.as_ref()).expect("write name to header");
            hdu
        } else {
            fits.create_image(name.as_ref(), &image_description).expect("extended hdu")
        };

        if TypeId::of::<T>() == TypeId::of::<u64>() && format == ImageType::LongLong {
            // FITS doesn't exactly have a concept of unsigned data in data blocks
            // it's all stored as signed and stored with an offset key in the header to make up the difference
            // cfitsio has helpers to write u16 and u32 images that are exposed to the rust wrapper,
            // but we need to implement this feature manually for u64
            hdu.write_key(fits, "BZERO", u64::MAX / 2).expect("write offset to header");
        }
        hdu.write_image(fits, arr.as_standard_layout().as_slice_memory_order().unwrap())
            .expect("write image data");
    }

    let mut fits = None;
    let out: String = args.value_from_str("--out").expect("--out");
    for (i, image) in xisf.images().enumerate() {
        let data = image.read_data(&xisf).expect(format!("read image {i} data").as_str());
        let name = image.id.clone().unwrap_or(format!("IMAGE_{i}"));
        match data {
            ImageData::UInt8(arr) => write_array(&mut fits, &out, &arr, name, ImageType::UnsignedByte),
            ImageData::UInt16(arr) => write_array(&mut fits, &out, &arr, name, ImageType::UnsignedShort),
            ImageData::UInt32(arr) => write_array(&mut fits, &out, &arr, name, ImageType::UnsignedLong),
            ImageData::UInt64(arr) => write_array(&mut fits, &out, &arr, name, ImageType::LongLong),
            ImageData::Float32(arr) => write_array(&mut fits, &out, &arr, name, ImageType::Float),
            ImageData::Float64(arr) => write_array(&mut fits, &out, &arr, name, ImageType::Double),
            ImageData::Complex32(_) | ImageData::Complex64(_) => eprintln!("FITS does not support complex images"),
        }
    }
}