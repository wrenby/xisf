# xisf-rs

An unaffiliated implementation of Pleiades Astrophoto's open-source Extensible Image Serialization Format (XISF) file format, the native image format for their flagship editing software PixInsight. Aims for 100% support for [spec version 1.0](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html), as opposed to implementations such as [libXISF](https://gitea.nouspiro.space/nou/libXISF) or Pleiades Astrophoto's own [PixInsight Class Libraries](https://gitlab.com/pixinsight/PCL), which are written with 2D images in mind. Currently under rapid development, and the API is changing rapidly, but all changes to the public API will be accompanied with a version update following [Cargo's SemVer guidelines](https://doc.rust-lang.org/cargo/reference/semver.html) before being pushed to [crates.io](https://crates.io/crates/xisf-rs).

## Feature Comparison

&nbsp; | `xisf-rs` | `libXISF` | `PCL`
---|:---:|:---:|:---:
Language | Rust | C++ | C++
License | MIT | GPLv3 | Custom
Monolithic Files | Decode | Encode + Decode | Encode + Decode
Distributed Files | :x: | :x: | :x:
N-D Images | :white_check_mark: | [:x:](https://gitea.nouspiro.space/nou/libXISF/src/commit/8e05a586109a634e3a43aeecc4ca693d00c2104e/libxisf.cpp#L816) | [:x:](https://gitlab.com/pixinsight/PCL/-/blob/7cd5ee14f6b209cf03f5b2d1903941ea1a4c8aec/src/pcl/XISFReader.cpp#L2001)
Pixel Sample Formats | Scalar | Agnostic (Raw Bytes Only) | Scalar ([except 64-bit integers](https://gitlab.com/pixinsight/PCL/-/blob/7cd5ee14f6b209cf03f5b2d1903941ea1a4c8aec/src/pcl/XISFReader.cpp#L599)), Complex
Image Metadata | Attributes only | FITS Keywords, XISF Properties, Thumbnail, CFA, ICC Profile | [Missing imageType, offset, orientation, and uuid attributes](https://gitea.nouspiro.space/nou/libXISF/src/commit/8e05a586109a634e3a43aeecc4ca693d00c2104e/libxisf.cpp#L815)
Supported XISF Property Locations | :x: | `<Image>`<sup>1, 2</sup> | `<Image>`, `<Metadata>`<sup>1</sup>
Data Block Compression | `zlib`, `lz4`, `lz4hc`<sup>3</sup> | `zlib`, `lz4`, `lz4hc`, `zstd`<sup>3, 4</sup> | `zlib`, `lz4`, `lz4hc`
Checksum Verification | :white_check_mark: | :x: | :white_check_mark:
`<Reference>` Element | :x: | :x: | :x:
XML Digital Signature Verification | :x: | :x: | :x:

1. Does not support `<Table>` properties
2. [Int32, Float32, Float64, String, and TimePoint only](https://gitea.nouspiro.space/nou/libXISF/src/commit/8e05a586109a634e3a43aeecc4ca693d00c2104e/variant.cpp#L379)
3. Sub-blocks not yet supported
4. `zstd` support is nonstandard

## Dependencies
- Minimum Supported Rust Version (MSRV): 1.64.0, verified for `x86_64-unknown-linux-gnu`
- libxml2
- lz4
- Can be configured to use zlib or zlib-ng

# Road Map

- [x] Read images: Only uncompressed scalar N-dimensional images for N &ge; 1
  - Supports `Planar` and `Normal` pixel storage modes
    - Consider not enforcing a read into `Planar`-organized memory: how to associate an image geometry with the array if its shape is no longer an indicator?
- [x] Data block compression
  - Not quite at baseline support: doesn't respect the `subblocks` attribute, meaning this implementation is limited to 4GiB files
  - Is something like a `MultiTake` or `VariableChunksIter` wrapper possible? YES: see `Take<T>::set_limit` ([proof of concept](https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=b6a55f09c803a8f3d48291b429455e46))
- [x] Checksum verification
- [ ] Make a decent public API instead of leaving everything `pub`
  - `ReadOptions` and `WriteOptions` should probably have Builders to avoid breaking changes if I add something new
  - Gate `ndarray` things behind an enabled-by-default feature, and expose a way to get raw `Vec`s
- [ ] Improve logging with span guards
- [ ] Write monolithic files
- [ ] Images of complex numbers
  - Spec doesn't actually clarify how these get serialized into binary, but `Complex` with C's complex types so I think I can just use that
  - Read into one big slice, then `ndarray::azip` macro on slices to construct the final complex? The cloning feels really unnecessary but it would definitely work. Could do that and improve it later.
  - Maybe implement my own trait wrapper á la `byteorder`?
  - Maybe do some memory magic, maybe not exactly `transmute` but something like it
  - Leading idea: integrate a helper library like `scroll` (`gread_inout` seems really promising) or `zerocopy`
- [ ] `<FITSKeyword>` element
- [ ] Scalar, Complex, String, and TimePoint `<Property>` elements
- [ ] Image thumbnails: turn `read_data` into a trait
- [ ] Documentation and tests
- [ ] `fitsio` interoperability feature
- [ ] `<Reference>` element: `impl MaybeReference for RoNode`
- [ ] CIE L\*a\*b color space conversion -- is this out of scope?
- [ ] 128-bit floating point `<Property>` types ([`rustc_apfloat::ieee::Quad`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/ieee/type.Quad.html)?)
  - Supports binary de/serialization with [`from_bits`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/trait.Float.html#tymethod.from_bits) and [`to_bits`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_apfloat/trait.Float.html#tymethod.to_bits), with the middle step of reading to `u128` first
  - Necessary because vectors and matrices are stored in data blocks
  - I could feasibly extend the spec to handle 128-bit pixel samples -- but does anyone actually want that?
- [ ] Remote resources
  - Mitigating security risks?? RCE hazard; some protocols may leak credentials on mount; all will leak IP
  - `remotefs` crate? Covers (S)FTP+SCP+SMB+S3, that plus an HTTP(S) client should cover most use cases
  - Some kind of file cache to avoid re-downloading? Check for changes in file size and last modified with `stat` to ensure up-to-date, and make an option to re-download a specific file in `DataBlock`'s read functions
    - HTTP(S) supports a more fine-grained cache with `ETag`/`If-None-Match`, `Last-Modified`/`If-Modified-Since` and `Cache-Control` headers
    - Consider `tempfile`, `memmap2` crates
- [ ] Vector, Matrix `<Property>` elements
  - Need to look at `cfitsio` for inspiration here
- [ ] XML Digital Signature verification
- [ ] async data block read functions
  - See `async_compression` crate
- [ ] C/C++11 interface with `cbindgen`
- [ ] zstd support? Nonstandard, but `libXISF` is doing it, and that seems to be the implementation that's catching on in the C++ world