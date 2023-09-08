use ndarray::{ArrayD, CowArray, IxDyn};
use num_complex::Complex;
use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut}
};
use super::PixelStorage;

/// An `enum` wrapper for images of all possible [`SampleFormat`](super::SampleFormat)s
///
/// If you think you know the sample format of a given image, or your program only accepts data of a certain type,
/// you can attempt to convert a `DynImageData` into that type with `let raw: RawImageData<TYPE> = image.read_data(&xisf).try_into();`
#[derive(Clone, Debug)]
pub enum DynImageData {
    UInt8(ImageData<u8>),
    UInt16(ImageData<u16>),
    UInt32(ImageData<u32>),
    UInt64(ImageData<u64>),
    Float32(ImageData<f32>),
    Float64(ImageData<f64>),
    Complex32(ImageData<Complex<f32>>),
    Complex64(ImageData<Complex<f64>>),
}
macro_rules! try_into_impl {
    ($enum:ident, $t:ty) => {
        #[doc=concat!("Returns `Ok(RawImageData<", stringify!($t), ">)` for [`", stringify!($enum), "`](Self::", stringify!($enum), ") variants, and `Err(())` otherwise")]
        impl TryInto<ImageData<$t>> for DynImageData {
            type Error = ();
            fn try_into(self) -> Result<ImageData<$t>, Self::Error> {
                match self {
                    Self::$enum(raw) => Ok(raw),
                    _ => Err(()),
                }
            }
        }
    }
}

try_into_impl!(UInt8, u8);
try_into_impl!(UInt16, u16);
try_into_impl!(UInt32, u32);
try_into_impl!(UInt64, u64);
try_into_impl!(Float32, f32);
try_into_impl!(Float64, f64);
try_into_impl!(Complex32, Complex<f32>);
try_into_impl!(Complex64, Complex<f64>);

/// Would ideally just be an [`Into`] implementation, but it needs to know the pixel storage as
/// supplementary information, which isn't in [`Into::into()`]'s function signature
pub(crate) trait IntoDynImageData {
    fn into_dyn_img(self, layout: PixelStorage) -> DynImageData;
}
macro_rules! into_impl {
    ($enum:ident, $t:ty) => {
        impl IntoDynImageData for ArrayD<$t> {
            /// Wraps the buffer inside of a [`DynRawImageData`]
            ///
            /// # Panics
            /// If the image is 0-dimensional (should be impossible when parsed from this library).
            /// An array can have elements in it and still be considered 0-dimensional,
            /// as one of the axes is considered a channel instead of a dimension.
            fn into_dyn_img(self, layout: PixelStorage) -> DynImageData {
                assert!(self.shape().len() >= 2);
                DynImageData::$enum(ImageData::<$t>::new(self, layout))
            }
        }
    }
}
into_impl!(UInt8, u8);
into_impl!(UInt16, u16);
into_impl!(UInt32, u32);
into_impl!(UInt64, u64);
into_impl!(Float32, f32);
into_impl!(Float64, f64);
into_impl!(Complex32, Complex<f32>);
into_impl!(Complex64, Complex<f64>);

use memory_layout::*;

/// A wrapper around the raw pixel data with some compile-time checks to avoid unintentionally ignoring its [memory layout](PixelStorage)
///
/// For an image with *N* dimensions, the inner [`ArrayD<T>`] has *N + 1* axes,
/// with either the first or last axis encoding the number of channels, depending on the layout:
/// - If `L` is [`Planar`]: The **first** axis of the inner [`ArrayD<T>`] is the channel axis, and the remaining *N* axes are image dimensions.
/// - If `L` is [`Normal`]: The **last** axis of the inner [`ArrayD<T>`] is the channel axis, and the previous *N* axes are image dimensions.
/// - If `L` is [`Raw`] or [`Accept`]: This data is arranged however it was in the file, according to the [`Image`](super::Image)'s .
///
/// Allows transparent access to the inner buffer through [`Deref`] and [`DerefMut`] only if `L` is `Planar`, `Normal`, or `Accept`.
/// Since all images are returned with `L = Raw` when read from file, this forces the end user to make a conscious decision to either
/// accept the responsibility of handling multiple image formats with [`Self::accept_current_layout()`] or transform it into a specific layout
/// their program is equipped to handle with [`Self::into_planar_layout()`] or [`Self::into_normal_layout()`].
///
/// For more information about pixel memory layouts, see the documentation for [`PixelStorage`]
///
/// # Example
///
/// ```
/// use xisf_rs::{XISF, image::RawImageData};
/// use ndarray::s;
/// let (xisf, ctx) = XISF::read_file("tests/files/2ch.xisf", &Default::default()).expect("failed to read file");
/// let img = xisf.get_image(0).read_data(&ctx).expect("failed to read image");
/// let raw: ImageData<u16> = img.try_into().expect("not u16 samples");
/// let planar = raw.to_planar_layout();
/// let normal = raw.to_normal_layout();
/// assert_eq!(planar.shape(), &[2, 10, 4]);
/// assert_eq!(normal.shape(), &[10, 4, 2]);
/// for c in 0..2 {
///     assert_eq!(planar.slice(s![c, .., ..]), normal.slice(s![.., .., c]));
/// }
/// ```
#[derive(Debug)]
pub struct ImageData<T, L: Layout = Raw> {
    inner: ArrayD<T>,
    /// Uses an associated type to save some memory once the layout is known.
    /// When the data is first read and `L` is [`Raw`], `layout` will store a [`PixelStorage`] value.
    /// After the data has been transformed into a specific memory layout with [`Self::into_planar_layout()`],
    /// `layout` will just be [`PhantomData`]
    /// since the layout is encoded in the template parameter, and storing it here would be redundant.
    layout: L::Storage,
}
impl<T, L: Layout> Clone for ImageData<T, L> where T: Clone {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone(), layout: self.layout.clone() }
    }
}
impl<T, L: Layout> ImageData<T, L> where T: Clone {
    /// Reorganizes the pixel samples into [planar layout](PixelStorage::Planar), consuming `self`
    ///
    /// - If the current memory layout is normal, enough memory is needed to temporarily duplicate the image
    /// - If the current memory layout is already planar, this is a no-op.
    pub fn into_planar_layout(self) -> ImageData<T, Planar> {
        L::into_planar(self)
    }
    /// Reorganizes the pixel samples into [normal layout](PixelStorage::Normal), consuming `self`
    ///
    /// If the current memory layout is already normal, this is a no-op.
    pub fn into_normal_layout(self) -> ImageData<T, Normal> {
        L::into_normal(self)
    }
    /// Reorganizes the pixel samples into [planar layout](PixelStorage::Planar),
    /// returning a copy-on-write (Cow) view of the underlying memory buffer
    ///
    /// - If the memory layout is already planar, the underlying memory buffer of the output
    /// Cow points to the same memory as `self` for operations which don't require mutable access.
    /// (That is, `std::ptr::eq(planar.to_slice(), planar.to_planar_layout().to_slice())) == true`.)
    /// When any method that requires mutable access to the output buffer is called, the buffer is cloned.
    /// - If the current memory layout is normal, the buffer is cloned immediately upon conversion.
    /// <div class="warning">
    ///
    /// Only recommended for use when owned access is impossible, since this will unnecessarily duplicate an
    /// arbitrarily large image buffer if mutable access to the buffer is required later.
    /// For most cases, use [`Self::into_planar_layout()`].
    ///
    /// </div>
    pub fn to_planar_layout(&self) -> CowImageData<T, Planar> {
        L::to_planar(self)
    }
    /// Reorganizes the pixel samples into [normal layout](PixelStorage::Normal),
    /// returning a copy-on-write (Cow) view of the underlying memory buffer
    ///
    /// - If the memory layout is already normal, the underlying memory buffer of the output
    /// Cow points to the same memory as `self` for operations which don't require mutable access.
    /// (That is, `std::ptr::eq(normal.to_slice(), normal.to_normal_layout().to_slice())) == true`.)
    /// When any method that requires mutable access to the output buffer is called, the buffer is cloned.
    /// - If the current memory layout is planar, the buffer is cloned immediately upon conversion.
    /// <div class="warning">
    ///
    /// Only recommended for use when owned access is impossible, since this will unnecessarily duplicate an
    /// arbitrarily large image buffer if mutable access to the buffer is required later.
    /// For most cases, use [`Self::into_normal_layout()`].
    ///
    /// </div>
    pub fn to_normal_layout(&self) -> CowImageData<T, Normal> {
        L::to_normal(self)
    }
    /// Returns the current memory layout
    pub fn layout(&self) -> PixelStorage {
        L::layout(&self)
    }
    /// Returns the number of channels in the image
    pub fn num_channels(&self) -> usize {
        match self.layout() {
            PixelStorage::Planar => *self.inner.shape().first().unwrap(),
            PixelStorage::Normal => *self.inner.shape().last().unwrap(),
        }
    }
    /// Returns the number of dimensions in the image
    pub fn num_dimensions(&self) -> usize {
        self.inner.shape().len() - 1
    }
}
impl<T> ImageData<T, Raw> {
    pub fn new(buf: ArrayD<T>, layout: PixelStorage) -> Self {
        Self {
            inner: buf,
            layout,
        }
    }
    /// Allows a method of accessing the underlying buffer without risking a memory clone
    pub fn accept_current_layout(self) -> ImageData<T, Accept> {
        ImageData::<T, Accept> {
            inner: self.inner,
            layout: self.layout,
        }
    }
}
impl<T, L: Known> Deref for ImageData<T, L> {
    type Target = ArrayD<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<T, L: Known> DerefMut for ImageData<T, L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Clone, Debug)]
pub struct CowImageData<'a, T, L: Layout> {
    inner: CowArray<'a, T, IxDyn>,
    layout: L::Storage,
}
impl<'a, T, L: Layout> CowImageData<'a, T, L> where T: Clone {
    pub fn to_owned(&self) -> ImageData<T, L> {
        ImageData::<T, L> {
            inner: self.inner.to_owned(),
            layout: self.layout.clone(),
        }
    }
}
impl<'a, T, L: Layout> From<&'a ImageData<T, L>> for CowImageData<'a, T, L> {
    fn from(value: &'a ImageData<T, L>) -> Self {
        Self {
            inner: (&value.inner).into(),
            layout: value.layout.clone(),
        }
    }
}
impl<'a, T, L: Layout> From<ImageData<T, L>> for CowImageData<'a, T, L> {
    fn from(value: ImageData<T, L>) -> Self {
        Self {
            inner: value.inner.into(),
            layout: value.layout.clone(),
        }
    }
}
impl<'a, T, L: Known> Deref for CowImageData<'a, T, L> {
    type Target = CowArray<'a, T, IxDyn>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<'a, T, L: Known> DerefMut for CowImageData<'a, T, L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// A small library of functions and types to simplify management of different [memory layouts](PixelStorage)
pub mod memory_layout {
    use super::*;
    use std::fmt::Debug;
    pub trait Layout: Sized {
        type Storage: Clone + Debug;
        fn into_planar<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Planar>;
        fn into_normal<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Normal>;
        fn to_planar<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Planar>;
        fn to_normal<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Normal>;
        fn layout<T>(img: &ImageData<T, Self>) -> PixelStorage;
    }

    // TODO: investigate alternatives with lower memory overhead
    // there's no way to do this without some kind of a buffer, but I don't need to duplicate the whole image
    // I think iterating axis by axis would work, but it would lose cache coherency
    // - did it ever have it? probably not. check `as_standard_layout()` implementation
    fn normal_to_planar<T>(buf: ArrayD<T>) -> ArrayD<T> where T: Clone {
        let num_axes = buf.shape().len();
        let mut indices: Vec<_> = (0..num_axes).into_iter().collect();
        indices.rotate_right(1);

        buf.permuted_axes(indices.as_slice())
            .as_standard_layout()
            .to_owned()
    }
    fn planar_to_normal<T>(buf: ArrayD<T>) -> ArrayD<T> where T: Clone {
        let num_axes = buf.shape().len();
        let mut indices: Vec<_> = (0..num_axes).into_iter().collect();
        indices.rotate_left(1);

        buf.permuted_axes(indices.as_slice())
            .as_standard_layout()
            .to_owned()
    }

    #[derive(Clone, Debug)]
    pub struct Planar;
    impl Layout for Planar {
        type Storage = PhantomData<Self>;

        #[inline]
        fn into_planar<T>(img: ImageData<T, Self>) -> ImageData<T, Planar> {
            img
        }

        fn into_normal<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Normal> {
            ImageData::<T, Normal> {
                inner: planar_to_normal(img.inner),
                layout: PhantomData,
            }
        }

        #[inline]
        fn to_planar<T>(img: &ImageData<T, Self>) -> CowImageData<T, Planar> {
            img.into()
        }

        #[inline]
        fn to_normal<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Normal> {
            Self::into_normal(img.clone()).into()
        }

        #[inline]
        fn layout<T>(_img: &ImageData<T, Self>) -> PixelStorage {
            PixelStorage::Planar
        }
    }

    #[derive(Clone, Debug)]
    pub struct Normal;
    impl Layout for Normal {
        type Storage = PhantomData<Self>;

        fn into_planar<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Planar> {
            ImageData::<T, Planar> {
                inner: normal_to_planar(img.inner),
                layout: PhantomData,
            }
        }

        #[inline]
        fn into_normal<T>(img: ImageData<T, Self>) -> ImageData<T, Normal> {
            img
        }

        #[inline]
        fn to_planar<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Planar> {
            Self::into_planar(img.clone()).into()
        }

        #[inline]
        fn to_normal<T>(img: &ImageData<T, Self>) -> CowImageData<T, Normal> {
            img.into()
        }

        #[inline]
        fn layout<T>(_img: &ImageData<T, Self>) -> PixelStorage {
            PixelStorage::Normal
        }
    }

    #[derive(Clone, Debug)]
    pub struct Raw;
    impl Layout for Raw {
        type Storage = PixelStorage;
        fn into_planar<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Planar> {
            ImageData::<T, Planar> {
                inner: match img.layout {
                    PixelStorage::Planar => img.inner,
                    PixelStorage::Normal => normal_to_planar(img.inner),
                },
                layout: PhantomData,
            }
        }

        fn into_normal<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Normal> {
            ImageData::<T, Normal> {
                inner: match img.layout {
                    PixelStorage::Planar => planar_to_normal(img.inner),
                    PixelStorage::Normal => img.inner,
                },
                layout: PhantomData,
            }
        }

        fn to_planar<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Planar> {
            CowImageData::<T, Planar> {
                inner: match img.layout {
                    PixelStorage::Planar => (&img.inner).into(),
                    PixelStorage::Normal => normal_to_planar(img.inner.clone()).into(),
                },
                layout: PhantomData,
            }
        }

        fn to_normal<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Normal> {
            CowImageData::<T, Normal> {
                inner: match img.layout {
                    PixelStorage::Planar => planar_to_normal(img.inner.clone()).into(),
                    PixelStorage::Normal => (&img.inner).into(),
                },
                layout: PhantomData,
            }
        }

        #[inline]
        fn layout<T>(img: &ImageData<T, Self>) -> PixelStorage {
            img.layout
        }
    }

    /// The exact same thing as [`Raw`] except [`ImageData<T, AcceptCurrent>`] implements [`Deref`], meaning its inner data can be accessed
    #[derive(Clone, Debug)]
    pub struct Accept;
    impl Layout for Accept {
        type Storage = <Raw as Layout>::Storage;

        fn into_planar<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Planar> {
            ImageData::<T, Planar> {
                inner: match img.layout {
                    PixelStorage::Planar => img.inner,
                    PixelStorage::Normal => normal_to_planar(img.inner),
                },
                layout: PhantomData,
            }
        }

        fn into_normal<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Normal> {
            ImageData::<T, Normal> {
                inner: match img.layout {
                    PixelStorage::Planar => planar_to_normal(img.inner),
                    PixelStorage::Normal => img.inner,
                },
                layout: PhantomData,
            }
        }

        fn to_planar<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Planar> {
            CowImageData::<T, Planar> {
                inner: match img.layout {
                    PixelStorage::Planar => (&img.inner).into(),
                    PixelStorage::Normal => normal_to_planar(img.inner.clone()).into(),
                },
                layout: PhantomData,
            }
        }

        fn to_normal<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Normal> {
            CowImageData::<T, Normal> {
                inner: match img.layout {
                    PixelStorage::Planar => planar_to_normal(img.inner.clone()).into(),
                    PixelStorage::Normal => (&img.inner).into(),
                },
                layout: PhantomData,
            }
        }

        fn layout<T>(img: &ImageData<T, Self>) -> PixelStorage {
            img.layout
        }
    }

    pub(crate) trait Known: Layout {}
    impl Known for Planar {}
    impl Known for Normal {}
    impl Known for Accept {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;
    use ndarray::{s, Array3, ArrayD};

    #[test]
    fn cow_no_premature_copy() {
        // doesn't actually matter what's in the array since we're only testing reference equality, so just make it a single value
        let arr = ArrayD::zeros(IxDyn(&[1]));

        let mut cow = CowImageData::<u16, Accept> {
            inner: (&arr).into(),
            layout: PixelStorage::Planar,
        };
        let inner = cow.inner.as_slice().unwrap().as_ptr();
        let inner_clone = cow.inner.clone().as_slice().unwrap().as_ptr();
        let clone = cow.clone().as_slice().unwrap().as_ptr();
        let to_owned = cow.to_owned().as_slice().unwrap().as_ptr();
        let deref = cow.deref().as_slice().unwrap().as_ptr();
        let mut_deref = cow.deref_mut().as_slice().unwrap().as_ptr();
        let mut_slice = cow.deref_mut().as_slice_mut().unwrap().as_ptr();
        assert!(ptr::eq(inner, inner_clone));
        // cloning a borrowed cow just clones the reference, not the data
        assert!(ptr::eq(inner_clone, clone));
        assert!(!ptr::eq(clone, to_owned));
        assert!(ptr::eq(clone, deref));
        // the data only clones when the inner buffer is accessed mutably, not when deref_mut() is called
        assert!(ptr::eq(deref, mut_deref));
        assert!(!ptr::eq(mut_deref, mut_slice));

        // no-copy conversions to planar layout

        let planar = ImageData::<u16, Planar> {
            inner: arr.clone(),
            layout: PhantomData,
        };
        let mut cow = planar.to_planar_layout();
        assert!(ptr::eq(planar.as_slice().unwrap(), cow.as_slice().unwrap()));
        // should clone when we request write access with as_slice_mut()
        assert!(!ptr::eq(planar.as_slice().unwrap(), cow.as_slice_mut().unwrap()));

        let raw_planar = ImageData::<u16, Raw> {
            inner: arr.clone(),
            layout: PixelStorage::Planar,
        };
        let mut cow = raw_planar.to_planar_layout();
        assert!(ptr::eq(raw_planar.inner.as_slice().unwrap(), cow.as_slice().unwrap()));
        // should clone when we request write access with as_slice_mut()
        assert!(!ptr::eq(raw_planar.inner.as_slice().unwrap(), cow.as_slice_mut().unwrap()));

        let accept_planar = raw_planar.accept_current_layout();
        let mut cow = accept_planar.to_planar_layout();
        assert!(ptr::eq(accept_planar.as_slice().unwrap(), cow.as_slice().unwrap()));
        // should clone when we request write access with as_slice_mut()
        assert!(!ptr::eq(accept_planar.as_slice().unwrap(), cow.as_slice_mut().unwrap()));

        // copying conversions to planar layout

        let normal = ImageData::<u16, Normal> {
            inner: arr.clone(),
            layout: PhantomData,
        };
        let cow = normal.to_planar_layout();
        assert!(!ptr::eq(normal.as_slice().unwrap(), cow.as_slice().unwrap()));

        let raw_normal = ImageData::<u16, Raw> {
            inner: arr.clone(),
            layout: PixelStorage::Normal,
        };
        let cow = raw_normal.to_planar_layout();
        assert!(!ptr::eq(raw_normal.inner.as_slice().unwrap(), cow.as_slice().unwrap()));

        let accept_normal = raw_normal.accept_current_layout();
        let cow = accept_normal.to_planar_layout();
        assert!(!ptr::eq(accept_normal.as_slice().unwrap(), cow.as_slice().unwrap()));

        // no-copy conversions to normal layout

        let normal = ImageData::<u16, Normal> {
            inner: arr.clone(),
            layout: PhantomData,
        };
        let mut cow = normal.to_normal_layout();
        assert!(ptr::eq(normal.as_slice().unwrap(), cow.as_slice().unwrap()));
        // should clone when we request write access with as_slice_mut()
        assert!(!ptr::eq(normal.as_slice().unwrap(), cow.as_slice_mut().unwrap()));

        let raw_normal = ImageData::<u16, Raw> {
            inner: arr.clone(),
            layout: PixelStorage::Normal,
        };
        let mut cow = raw_normal.to_normal_layout();
        assert!(ptr::eq(raw_normal.inner.as_slice().unwrap(), cow.as_slice().unwrap()));
        // should clone when we request write access with as_slice_mut()
        assert!(!ptr::eq(raw_normal.inner.as_slice().unwrap(), cow.as_slice_mut().unwrap()));

        let accept_normal = raw_normal.accept_current_layout();
        let mut cow = accept_normal.to_normal_layout();
        assert!(ptr::eq(accept_normal.as_slice().unwrap(), cow.as_slice().unwrap()));
        // should clone when we request write access with as_slice_mut()
        assert!(!ptr::eq(accept_normal.as_slice().unwrap(), cow.as_slice_mut().unwrap()));

        // copying conversions to normal layout

        let arr = ArrayD::zeros(IxDyn(&[256, 256]));
        let planar = ImageData::<u16, Planar> {
            inner: arr.clone(),
            layout: PhantomData,
        };
        let cow = planar.to_normal_layout();
        assert!(!ptr::eq(planar.as_slice().unwrap(), cow.as_slice().unwrap()));

        let raw_planar = ImageData::<u16, Raw> {
            inner: arr.clone(),
            layout: PixelStorage::Planar,
        };
        let cow = raw_planar.to_normal_layout();
        assert!(!ptr::eq(raw_planar.inner.as_slice().unwrap(), cow.as_slice().unwrap()));

        let accept_planar = raw_planar.accept_current_layout();
        let cow = accept_planar.to_normal_layout();
        assert!(!ptr::eq(accept_planar.as_slice().unwrap(), cow.as_slice().unwrap()));
    }

    #[test]
    fn layout_conversions() {
        // https://stackoverflow.com/a/56762490
        let mut array: Array3<u8> = Array3::zeros((200, 250, 3)); // 200x250 RGB
        for ((x, y, z), v) in array.indexed_iter_mut() {
            *v = match z {
                0 => y as u8,
                1 => x as u8,
                2 => 255 - (x as u8).min(y as u8),
                _ => unreachable!(),
            };
        }

        macro_rules! test_with_origin {
            ($l:ty, $layout:expr, $sanity:expr) => {
                let raw = ImageData::<u8, $l> {
                    inner: array.clone().into_dyn(),
                    layout: $layout,
                };
                assert_eq!(raw.layout(), $sanity);

                let into_planar = raw.clone().into_planar_layout();
                let into_normal = raw.clone().into_normal_layout();
                let to_planar = raw.to_planar_layout();
                let to_normal = raw.to_normal_layout();
                assert_eq!(into_planar.shape(), &[3, 200, 250]);
                assert_eq!(into_normal.shape(), &[200, 250, 3]);
                assert_eq!(to_planar.shape(), &[3, 200, 250]);
                assert_eq!(to_normal.shape(), &[200, 250, 3]);
                assert_eq!(into_planar.layout(), PixelStorage::Planar);
                assert_eq!(into_normal.layout(), PixelStorage::Normal);
                assert_eq!(into_planar.num_channels(), 3);
                assert_eq!(into_normal.num_channels(), 3);
                assert_eq!(into_planar.num_dimensions(), 2);
                assert_eq!(into_normal.num_dimensions(), 2);
                // TODO
                // assert_eq!(to_planar.layout(), PixelStorage::Planar);
                // assert_eq!(to_normal.layout(), PixelStorage::Normal);
                for c in 0..3 {
                    // checks equivalence across to_planar -> to_normal -> into_planar -> into_normal
                    assert!(to_planar.slice(s![c, .., ..]) == to_normal.slice(s![.., .., c]));
                    assert_eq!(to_normal.slice(s![.., .., c]), into_planar.slice(s![c, .., ..]));
                    assert_eq!(into_planar.slice(s![c, .., ..]), into_normal.slice(s![.., .., c]));
                }
            }
        }

        test_with_origin!(Normal, PhantomData, PixelStorage::Normal);
        test_with_origin!(Raw, PixelStorage::Normal, PixelStorage::Normal);
        test_with_origin!(Accept, PixelStorage::Normal, PixelStorage::Normal);

        array = array.into_shape((3, 200, 250)).unwrap();

        test_with_origin!(Planar, PhantomData, PixelStorage::Planar);
        test_with_origin!(Raw, PixelStorage::Planar, PixelStorage::Planar);
        test_with_origin!(Accept, PixelStorage::Planar, PixelStorage::Planar);
    }

    #[test]
    fn dyn_image_conversions() {
        macro_rules! test_conversion {
            ($i:ident, $t:ty) => {
                let arr = ArrayD::<$t>::zeros(IxDyn(&[2, 3, 4, 5]));
                let there = arr.into_dyn_img(PixelStorage::Normal);
                assert!(matches!(there, DynImageData::$i(_)));
                let back_again: Result<ImageData<$t>, _> = there.try_into();
                assert!(back_again.is_ok());
            }
        }
        test_conversion!(UInt8, u8);
        test_conversion!(UInt16, u16);
        test_conversion!(UInt32, u32);
        test_conversion!(UInt64, u64);
        test_conversion!(Float32, f32);
        test_conversion!(Float64, f64);
        test_conversion!(Complex32, Complex<f32>);
        test_conversion!(Complex64, Complex<f64>);
    }
}