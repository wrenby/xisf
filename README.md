# xisf-rs

An unaffiliated implementation of Pleiades Astrophoto's open-source XISF file format, the not-so-new default for their flagship editing software PixInsight. Aims for 100% compatibility with [spec version 1.0](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html), as opposed to the [baseline](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#Conformance) support found in contemporary implementations such as [libXISF](https://gitea.nouspiro.space/nou/libXISF) or Pleiades Astrophoto's own [PixInsight Class Libraries](https://gitlab.com/pixinsight/PCL).

## Dependencies
- Are these build-time or runtime dependencies? Not sure, maybe both
- libxml2
- Relies on GIO to access remote resources in [distributed XISF units](https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html#distributed_xisf_unit), provided by the [GTK library](https://www.gtk.org/docs/installations/)

# Roadmap

- [ ] MVP loading of images: N-dimensional images for N &ge; 1, not just 2D with M channels
- [ ] Optional image attributes
- [ ] Load image thumbnails
- [ ] Image properties
- [ ] Remote resources via GIO
- [ ] zstd support
- [ ] XML Digital Signature verification