# xisf-rs

An unaffiliated implementation of Pleiades Astrophoto's open-source Extensible Image Serialization Format (XISF) file format, the native image format for their flagship editing software PixInsight. Aims for 100% support for [spec version 1.0](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html), as opposed to the [baseline](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#Conformance) support found in contemporary implementations such as [libXISF](https://gitea.nouspiro.space/nou/libXISF) or Pleiades Astrophoto's own [PixInsight Class Libraries](https://gitlab.com/pixinsight/PCL).

## Dependencies
- libxml2

# Road Map

- [x] Read images: Only uncompressed scalar N-dimensional images for N &ge; 1
  - Supports `Planar` and `Normal` pixel storage modes
- [ ] Remaining image attributes
- [ ] Data block compression
  - Baseline decoder support
  - Byte shuffling is fundamentally the same transformation as turning `Planar` pixel storage into `Normal`?
- [ ] Fix visibility/mutability of interior fields to ensure invariants hold, and improve public API to make up for the lost functionality
- [ ] Baseline encoder support
- [ ] Images of complex numbers
- [ ] `<FITSKeyword>`
- [ ] Scalar, Complex, String, TimePoint `<Property>`
- [ ] Image thumbnails
- [ ] `<Reference>`
- [ ] CIE L\*a\*b color space support
  - Expose a `ReadOption` to convert to RGB, on by default
- [ ] 128-bit floating point `<Property>` types ([`rustc_apfloat::ieee::Quad`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/ieee/type.Quad.html)?)
  - Supports binary de/serialization with [`from_bits`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/trait.Float.html#tymethod.from_bits) and [`to_bits`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/trait.Float.html#tymethod.to_bits), with the middle step of reading to `u128` first
  - Necessary because vectors and matrices are stored in data blocks
- [ ] Vector, Matrix, and Table `<Property>`
  - Need to look at `cfitsio` for inspiration here
- [ ] XML Digital Signature verification
- [ ] async data block read functions
- [ ] Remote resources
  - Mitigating security risks?? RCE hazard; some protocols may leak credentials on mount; all will leak IP
  - `remotefs` crate? Covers (S)FTP+SCP+SMB+S3, that plus an HTTP(S) client should cover most use cases
  - Some kind of file cache to avoid re-downloading? Check for changes in file size and last modified with `stat` to ensure up-to-date, and make an option to re-download a specific file in `DataBlock`'s read functions
    - HTTP(S) supports a more fine-grained cache with `ETag`/`If-None-Match`, `Last-Modified`/`If-Modified-Since` and `Cache-Control` headers
- [ ] C/C++11 interface with `cbindgen`
- [ ] zstd support? Nonstandard, but seems reasonable