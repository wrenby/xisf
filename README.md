# xisf-rs

An unaffiliated implementation of Pleiades Astrophoto's open-source Extensible Image Serialization Format (XISF) file format, the native image format for their flagship editing software PixInsight. Aims for 100% support for [spec version 1.0](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html), as opposed to the [baseline](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#Conformance) support found in contemporary implementations such as [libXISF](https://gitea.nouspiro.space/nou/libXISF) or Pleiades Astrophoto's own [PixInsight Class Libraries](https://gitlab.com/pixinsight/PCL).

## Dependencies
- I think these are build-time only?
- libxml2

# Roadmap

- [ ] MVP loading of images: N-dimensional images for N &ge; 1, not just 2D with M channels
- [ ] Optional image attributes
- [ ] Data block compression
- [ ] `<FITSKeyword>`
- [ ] Scalar, Complex, String, TimePoint `<Property>`
- [ ] Load image thumbnails
- [ ] `<Reference>`
- [ ] C/C++11 interface with `cbindgen`
- [ ] 128-bit floating point ([`rustc_apfloat::ieee::Quad`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/ieee/type.Quad.html)?)
  - Supports binary de/serialization with [`from_bits`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/trait.Float.html#tymethod.from_bits) and [`to_bits`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/trait.Float.html#tymethod.to_bits), with the middle step of
  - Necessary because vectors and matrices are stored in data blocks
- [ ] async data block read functions
- [ ] Vector, Matrix, and Table `<Property>`
- [ ] XML Digital Signature verification
  - Necessary for remote resources
- [ ] Remote resources
  - Mitigating security risks??: RCE hazard; some protocols may leak credentials on mount
  - [`remotefs`] crate? Covers (S)FTP+SCP+SMB+S3, that plus an HTTP(S) client should cover most use cases
  - Some kind of file cache to avoid re-downloading? Check for changes in file size and last modified with `stat` to ensure up-to-date, and make an option to re-download a specific file in `DataBlock`'s read functions
    - HTTP(S) supports a more fine-grained cache with `ETag`/`If-None-Match`, `Last-Modified`/`If-Modified-Since` and `Cache-Control` headers
- [ ] zstd support? Nonstandard, but seems reasonable