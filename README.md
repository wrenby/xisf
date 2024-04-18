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
License | MIT/Apache | GPLv3 | BSD-4-Clause-like
Monolithic Files | Read | Read + Write | Read + Write
Distributed Files | <details><summary>Read (Partial)</summary>`http`, `https`, `ftp`, see [road map](#road-map)</details> | ❌ | ❌
N-D Images | ✅ | [❌](https://gitea.nouspiro.space/nou/libXISF/src/commit/8e05a586109a634e3a43aeecc4ca693d00c2104e/libxisf.cpp#L816) | [❌](https://gitlab.com/pixinsight/PCL/-/blob/7cd5ee14f6b209cf03f5b2d1903941ea1a4c8aec/src/pcl/XISFReader.cpp#L2001)
Pixel Sample Formats | Scalar, Complex | Agnostic (Raw Bytes Only) | Scalar<sup>1</sup>, Complex
Image Metadata | <details><summary>Everything</summary> Attributes, XISF Properties, FITS Keywords, ICC Profile, RGB Working Space, Display Function, CFA, Resolution, Thumbnail</details> | <details><summary>Basic</summary> Attributes<sup>2</sup>, XISF Properties<sup>3</sup>, FITS Keywords<sup>4</sup>, ICC Profile, CFA, Thumbnail</details> | <details><summary>Nearly Everything</summary> Attributes<sup>5</sup>, XISF Properties<sup>6</sup>, FITS Keywords, ICC Profile, RGB Working Space, Display Function, CFA, Resolution, Thumbnail</details>
Supported XISF Property Locations | `<Image>`, `<Metadata>`, `<xisf>` | `<Image>` | `<Image>`, `<Metadata>`, `<xisf>`
`<Table>` Element | ✅ | ❌ | ❌
`<Reference>` Element | ✅ | ❌ | ❌
Data Block Compression | <details><summary>✅<sup>7</sup>&nbsp;&nbsp;</summary>`zlib`, `lz4`, `lz4hc`, `zstd`</details> | <details><summary>✅<sup>7, 8</sup></summary>`zlib`, `lz4`, `lz4hc`, `zstd`</details> | <details><summary>✅<sup>7</sup>&nbsp;&nbsp;</summary>`zlib`, `lz4`, `lz4hc`, `zstd`</details>
Checksum Verification | ✅ | ❌ | ✅
XML Digital Signature Verification | ❌ | ❌ | ❌

<details>
<summary>Footnotes</summary>

1. [Does not support 64-bit integer pixel samples](https://gitlab.com/pixinsight/PCL/-/blob/1e09795f1868835ddd221ac8605d3856c3208b57/src/pcl/XISFReader.cpp#L599)
2. [Only mandatory attributes and colorSpace](https://gitea.nouspiro.space/nou/libXISF/src/commit/8e05a586109a634e3a43aeecc4ca693d00c2104e/libxisf.cpp#L815)
3. [Int32, Float32, Float64, String, and TimePoint only](https://gitea.nouspiro.space/nou/libXISF/src/commit/8e05a586109a634e3a43aeecc4ca693d00c2104e/variant.cpp#L379)
4. Raw strings only; cannot parse values
5. [Missing imageType, offset, orientation, and uuid attributes](https://gitlab.com/pixinsight/PCL/-/blob/7cd5ee14f6b209cf03f5b2d1903941ea1a4c8aec/src/pcl/XISFReader.cpp#L674)
6. [No 128-bit integer or floating point values](https://gitlab.com/pixinsight/PCL/-/blob/1e09795f1868835ddd221ac8605d3856c3208b57/src/pcl/XISFReader.cpp#L1774)
7. `zstd` support is nonstandard for spec version 1.0, but [has been confirmed for an upcoming version of the standard](https://pixinsight.com/forum/index.php?threads/xisf-standard-revision-re-zstd.21230/)
8. Sub-blocks not yet supported (this limits supported images to 4GiB)
</details>

## Dependencies
- Minimum Supported Rust Version (MSRV): 1.67.0, verified for `x86_64-unknown-linux-gnu`
- libxml2 ([MIT](https://gitlab.gnome.org/GNOME/libxml2/-/blob/master/Copyright))
- lz4 ([BSD-2-Clause](https://github.com/lz4/lz4/blob/dev/LICENSE))
- zstd ([BSD-3-Clause](https://github.com/facebook/zstd/blob/dev/LICENSE))
- Can be configured to use zlib ([Zlib](https://github.com/madler/zlib/blob/develop/LICENSE)) or zlib-ng ([Zlib](https://github.com/zlib-ng/zlib-ng/blob/develop/LICENSE.md))
- gcc quadmath ([LGPL v2.1](https://github.com/gcc-mirror/gcc/blob/master/libquadmath/COPYING.LIB)) when the `f128` feature is enabled
  - quadmath doesn't support every CPU architecture, so 128-bit floating point properties are limited to those architectures
- Test suite requires a Docker installation when the `remote-ftp` feature is enabled

# Road Map

- [x] Read N-dimensional images
- [x] Data block compression
- [x] Checksum verification
- [x] Images of complex numbers
- [x] `<Reference>` element
- [x] `<FITSKeyword>` element
- [x] Image thumbnails
- [x] Remote resources
  - [x] Ask user for trust before connecting
  - [ ] Caching <!-- `tempfile`, `stat` crates -->
    - Files with a checksum are read through twice, and right now that means the file gets downloaded twice
  - [ ] Authorization/Credentials Store
    - Stateful schemes like FTP require keeping track of sessions
  - [ ] XISB files
  - [ ] SFTP/SCP
- [x] Non-`<Property>` image metadata
- [x] Scalar, Complex, String, and TimePoint `<Property>` elements
- [x] Vector, Matrix `<Property>` elements
- [x] 128-bit floating point `<Property>` types
- [ ] Write monolithic files
- [ ] `<Table>` element
- [ ] Property format strings
- [ ] Write distributed files
- [ ] Color space conversion?
- [ ] XML Digital Signature verification
- [ ] async data block read functions <!-- `async_compression` -->
- [ ] C/C++11 interface with `cbindgen`
