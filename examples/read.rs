use error_stack::{fmt::{Charset, ColorMode}, Report};
use tracing::Level;
use tracing_error::ErrorLayer;
use tracing_subscriber::{prelude::*, FmtSubscriber};
use xisf::image::ImageData;
use std::env;

fn main() {
    // TODO: honestly I'm undecided on whether I want to use stdout or stderr for the tracing subscriber's output

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

    let xisf = xisf::XISF::read_file("test-files/BiasStacked/test.xisf", &Default::default()).unwrap();
    println!("{xisf:#?}");

    let ndarray = xisf.images[0].read_data(&xisf).unwrap();
    println!("{:?}", ndarray);
    match ndarray {
        ImageData::Float32(arr) => {
            let image_description = fitsio::images::ImageDescription {
                data_type: fitsio::images::ImageType::Float,
                dimensions: xisf.images[0].geometry_trimmed(),
            };

            let mut fits = fitsio::FitsFile::create("test-files/BiasStacked/converted.fits")
                .overwrite()
                .with_custom_primary(&image_description)
                .open()
                .unwrap();
            fits.primary_hdu().unwrap().write_image(&mut fits, arr.as_slice().unwrap()).unwrap(); // arr.reversed_axes().as_slice_memory_order().unwrap()).unwrap();
        }
        _ => todo!(),
    }

}