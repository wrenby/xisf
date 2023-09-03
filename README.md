# xisf-rs

[![Crates.io](https://img.shields.io/crates/v/xisf-rs)](https://crates.io/crates/xisf-rs)
[![docs.rs](https://img.shields.io/docsrs/xisf-rs)](https://docs.rs/xisf-rs)
![Minimum rustc version](https://img.shields.io/badge/rustc-1.64+-lightgray.svg)
![License](https://img.shields.io/crates/l/xisf-rs.svg)

An unaffiliated implementation of Pleiades Astrophoto's open-source Extensible Image Serialization Format (XISF) file format, the native image format for their flagship editing software PixInsight. Aims for 100% support for [spec version 1.0](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html), as opposed to implementations such as [libXISF](https://gitea.nouspiro.space/nou/libXISF) or Pleiades Astrophoto's own [PixInsight Class Libraries](https://gitlab.com/pixinsight/PCL), which are written with 2D images in mind. Currently under rapid development, but all changes to the public API will be accompanied with a version update following [Cargo's SemVer guidelines](https://doc.rust-lang.org/cargo/reference/semver.html) before being pushed to [crates.io](https://crates.io/crates/xisf-rs).

## Feature Comparison

&nbsp; | `xisf-rs` | `libXISF` | `PCL`
---|:---:|:---:|:---:
Language | Rust | C++ | C++
License | MIT | GPLv3 | BSD-4-Clause-like
Monolithic Files | Decode | Encode + Decode | Encode + Decode
Distributed Files | <details><summary>Decode (Partial)</summary>Supported schemes: `file`, `http`/`https` (basic auth not supported), and `ftp` (credentials must be encoded in URI). No local caching; no XISB files.</details> | ❌ | ❌
N-D Images | ✅ | [❌](https://gitea.nouspiro.space/nou/libXISF/src/commit/8e05a586109a634e3a43aeecc4ca693d00c2104e/libxisf.cpp#L816) | [❌](https://gitlab.com/pixinsight/PCL/-/blob/7cd5ee14f6b209cf03f5b2d1903941ea1a4c8aec/src/pcl/XISFReader.cpp#L2001)
Pixel Sample Formats | Scalar, Complex | Agnostic (Raw Bytes Only) | Scalar<sup>1</sup>, Complex
Image Metadata | <details>Attributes, FITS Keywords, ICC Profile, RGB Working Space, Display Function, CFA</details> | <details>Attributes<sup>2</sup>, XISF Properties<sup>3</sup>, FITS Keywords<sup>4</sup>, ICC Profile, CFA, Thumbnail</details> | <details>Attributes<sup>5</sup>, XISF Properties, FITS Keywords, ICC Profile, RGB Working Space, Display Function, CFA, Resolution, Thumbnail</details>
Supported XISF Property Locations | ❌ | `<Image>` | `<Image>`, `<Metadata>`, `<xisf>`
`<Table>` Element | ❌ | ❌ | ❌
`<Reference>` Element | ✅ | ❌ | ❌
Data Block Compression | <details><summary>✅<sup>6</sup></summary>`zlib`, `lz4`, `lz4hc`, `zstd`</details> | <details><summary>✅<sup>6, 7</sup></summary>`zlib`, `lz4`, `lz4hc`, `zstd`</details> | <details><summary>✅<sup>6</sup></summary>`zlib`, `lz4`, `lz4hc`, `zstd`</details>
Checksum Verification | ✅ | ❌ | ✅
XML Digital Signature Verification | ❌ | ❌ | ❌

1. [Does not support 64-bit integers](https://gitlab.com/pixinsight/PCL/-/blob/7cd5ee14f6b209cf03f5b2d1903941ea1a4c8aec/src/pcl/XISFReader.cpp#L599)
2. [Only mandatory attributes and colorSpace](https://gitea.nouspiro.space/nou/libXISF/src/commit/8e05a586109a634e3a43aeecc4ca693d00c2104e/libxisf.cpp#L815)
3. [Int32, Float32, Float64, String, and TimePoint only](https://gitea.nouspiro.space/nou/libXISF/src/commit/8e05a586109a634e3a43aeecc4ca693d00c2104e/variant.cpp#L379)
4. Raw strings only; cannot parse values
5. [Missing imageType, offset, orientation, and uuid attributes](https://gitlab.com/pixinsight/PCL/-/blob/7cd5ee14f6b209cf03f5b2d1903941ea1a4c8aec/src/pcl/XISFReader.cpp#L674)
6. `zstd` support is nonstandard for spec version 1.0, but [has been confirmed for an upcoming version of the standard](https://pixinsight.com/forum/index.php?threads/xisf-standard-revision-re-zstd.21230/)
7. Sub-blocks not yet supported (this limits supported images to 4GiB)

## Dependencies
- Minimum Supported Rust Version (MSRV): 1.64.0, verified for `x86_64-unknown-linux-gnu`
- libxml2 (MIT)
- lz4 (BSD-2-Clause)
- zstd (BSD-3-Clause)
- Can be configured to use zlib or zlib-ng

# Road Map

- [x] Read N-dimensional images
- [x] Data block compression
- [x] Checksum verification
- [x] Images of complex numbers
- [x] `<Reference>` element
- [x] `<FITSKeyword>` element
- [ ] Miscellaneous image metadata
- [ ] Image thumbnails
- [ ] Write monolithic files
- [ ] Scalar, Complex, String, and TimePoint `<Property>` elements
- [ ] Documentation and tests
- [ ] CIE L\*a\*b color space conversion -- is this out of scope?
- [ ] Remote resources
  - Ask user to trust a specific source before connecting -- caching or permanently saving preferences per source address is out of scope
  - `remotefs` crate? Covers (S)FTP+SCP+SMB+S3, that plus an HTTP(S) client should cover most use cases. the SMB library is GPLv3 though :(
  - Some kind of file cache to avoid re-downloading? Consider `tempfile` crate. Check for changes in file size and last modified with `stat` to ensure up-to-date, and make an option to re-download a specific file in `DataBlock`'s read functions
    - HTTP(S) supports a more fine-grained cache with `ETag`/`If-None-Match`, `Last-Modified`/`If-Modified-Since` and `Cache-Control` headers
- [ ] Vector, Matrix `<Property>` elements
  - Need to look at `cfitsio` for inspiration here
- [ ] XML Digital Signature verification
- [ ] 128-bit floating point `<Property>` types ([`rustc_apfloat::ieee::Quad`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/ieee/type.Quad.html)? [`fixed::F128`](https://docs.rs/fixed/latest/fixed/struct.F128.html)?)
  - Neither support any kind of math, but `fixed::F128` supports `bytemuck`, which gives it an advantage
- [ ] async data block read functions
  - See `async_compression` crate
- [ ] C/C++11 interface with `cbindgen`