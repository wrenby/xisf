use ndarray::{ArrayD, CowArray, IxDyn};
use num_complex::Complex;
use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut}
};
use super::PixelStorage;

pub type RawImageData<T> = ImageData<T, Raw>;

/// An `enum` wrapper for images of all possible [`SampleFormat`](super::SampleFormat)s
#[derive(Clone, Debug)]
pub enum DynImageData {
    UInt8(RawImageData<u8>),
    UInt16(RawImageData<u16>),
    UInt32(RawImageData<u32>),
    UInt64(RawImageData<u64>),
    Float32(RawImageData<f32>),
    Float64(RawImageData<f64>),
    Complex32(RawImageData<Complex<f32>>),
    Complex64(RawImageData<Complex<f64>>),
}
macro_rules! try_into_impl {
    ($enum:ident, $t:ty) => {
        #[doc=concat!("Returns `Ok(RawImageData<", stringify!($t), ">)` for [`", stringify!($enum), "`](Self::", stringify!($enum), ") variants, and `Err(())` otherwise")]
        impl TryInto<RawImageData<$t>> for DynImageData {
            type Error = ();
            fn try_into(self) -> Result<RawImageData<$t>, Self::Error> {
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

pub(crate) trait IntoDynImageData {
    fn into_dyn_img(self, layout: PixelStorage) -> DynImageData;
}
macro_rules! into_impl {
    ($enum:ident, $t:ty) => {
        impl IntoDynImageData for ArrayD<$t> {
            fn into_dyn_img(self, layout: PixelStorage) -> DynImageData {
                DynImageData::$enum(RawImageData::<$t>::new(self, layout))
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
/// - If `L` is [`Raw`] or [`AcceptCurrent`]: This data is arranged however it was in the file.
///
/// Allows transparent access to the inner buffer through [`Deref`] and [`DerefMut`] only if `L` is `Planar`, `Normal`, or `AcceptCurrent`.
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
/// let xisf = XISF::read_file("tests/files/2ch.xisf", &Default::default()).expect("failed to read file");
/// let img = xisf.get_image(0).read_data(&xisf).expect("failed to read image");
/// let raw: RawImageData<u16> = img.try_into().expect("not u16 samples");
/// let planar = raw.to_planar_layout();
/// let normal = raw.to_normal_layout();
/// assert_eq!(planar.shape(), &[2, 10, 4]);
/// assert_eq!(normal.shape(), &[10, 4, 2]);
/// for c in 0..2 {
///     assert_eq!(planar.slice(s![c, .., ..]), normal.slice(s![.., .., c]));
/// }
/// ```
#[derive(Clone, Debug)]
pub struct ImageData<T: Clone, L: Layout> {
    inner: ArrayD<T>,
    /// Uses an associated type to save some memory once the layout is known.
    /// When the data is first read and `L` is [`Raw`], `layout` will store a [`PixelStorage`] value.
    /// After the data has been transformed into a specific memory layout with [`Self::into_planar_layout()`],
    /// `layout` will just be [`PhantomData`]
    /// since the layout is encoded in the template parameter, and storing it here would be redundant.
    layout: L::Storage,
}
impl<T: Clone, L: Layout> ImageData<T, L> {
    pub fn into_planar_layout(self) -> ImageData<T, Planar> {
        L::into_planar(self)
    }
    pub fn into_normal_layout(self) -> ImageData<T, Normal> {
        L::into_normal(self)
    }
    pub fn to_planar_layout(&self) -> CowImageData<T, Planar> {
        L::to_planar(self)
    }
    pub fn to_normal_layout(&self) -> CowImageData<T, Normal> {
        L::to_normal(self)
    }
    pub fn layout(&self) -> PixelStorage {
        L::layout(&self)
    }
}
impl<T: Clone> ImageData<T, Raw> {
    pub fn new(buf: ArrayD<T>, layout: PixelStorage) -> Self {
        Self {
            inner: buf,
            layout,
        }
    }
    /// Allows a method of accessing the underlying buffer without risking a memory clone
    pub fn accept_current_layout(self) -> ImageData<T, AcceptCurrent> {
        ImageData::<T, AcceptCurrent> {
            inner: self.inner,
            layout: self.layout,
        }
    }
}
impl<T: Clone, L: Known> Deref for ImageData<T, L> {
    type Target = ArrayD<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<T: Clone, L: Known> DerefMut for ImageData<T, L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug)]
pub struct CowImageData<'a, T: Clone, L: Layout> {
    inner: CowArray<'a, T, IxDyn>,
    layout: L::Storage,
}
impl<'a, T: Clone, L: Layout> CowImageData<'a, T, L> {
    pub fn borrowed(value: &'a ImageData<T, L>) -> Self {
        Self {
            inner: (&value.inner).into(),
            layout: value.layout.clone(),
        }
    }
    pub fn owned(value: ImageData<T, L>) -> Self {
        Self {
            inner: value.inner.into(),
            layout: value.layout,
        }
    }
    pub fn to_owned(&self) -> ImageData<T, L> {
        ImageData::<T, L> {
            inner: self.inner.to_owned(),
            layout: self.layout.clone(),
        }
    }
}
impl<'a, T: Clone, L: Known> Deref for CowImageData<'a, T, L> {
    type Target = CowArray<'a, T, IxDyn>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<'a, T: Clone, L: Known> DerefMut for CowImageData<'a, T, L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// A small library of functions and types to simplify management of different [memory layouts](PixelStorage)
pub mod memory_layout {
    use super::*;
    pub trait Layout: Sized {
        type Storage: Clone;
        fn into_planar<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Planar>;
        fn into_normal<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Normal>;
        fn to_planar<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Planar>;
        fn to_normal<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Normal>;
        fn layout<T: Clone>(img: &ImageData<T, Self>) -> PixelStorage;
    }

    fn normal_to_planar<T: Clone>(buf: ArrayD<T>) -> ArrayD<T> {
        let num_axes = buf.shape().len();
        let mut indices: Vec<_> = (0..num_axes).into_iter().collect();
        indices.rotate_right(1);

        buf.permuted_axes(indices.as_slice())
            .as_standard_layout()
            .to_owned()
    }
    fn planar_to_normal<T: Clone>(buf: ArrayD<T>) -> ArrayD<T> {
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
        fn into_planar<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Planar> {
            img
        }

        fn into_normal<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Normal> {
            ImageData::<T, Normal> {
                inner: planar_to_normal(img.inner),
                layout: PhantomData,
            }
        }

        #[inline]
        fn to_planar<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Planar> {
            CowImageData::<T, Planar>::borrowed(img)
        }

        #[inline]
        fn to_normal<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Normal> {
            CowImageData::<T, Normal>::owned(
                Self::into_normal(img.clone())
            )
        }

        #[inline]
        fn layout<T: Clone>(_img: &ImageData<T, Self>) -> PixelStorage {
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
        fn into_normal<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Normal> {
            img
        }

        #[inline]
        fn to_planar<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Planar> {
            CowImageData::<T, Planar>::owned(
                Self::into_planar(img.clone())
            )
        }

        #[inline]
        fn to_normal<T: Clone>(img: &ImageData<T, Self>) -> CowImageData<T, Normal> {
            CowImageData::<T, Normal>::borrowed(img.into())
        }

        #[inline]
        fn layout<T: Clone>(_img: &ImageData<T, Self>) -> PixelStorage {
            PixelStorage::Normal
        }
    }

    // shouldn't actually have to derive Clone or Debug here, but it's an easier fix than
    // manually implementing Clone and Debug for [`super::DynImageData`]
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
        fn layout<T: Clone>(img: &ImageData<T, Self>) -> PixelStorage {
            img.layout
        }
    }

    /// The exact same thing as [`Raw`] except [`ImageData<T, AcceptCurrent>`] implements [`Deref`], meaning its inner data can be accessed
    #[derive(Clone, Debug)]
    pub struct AcceptCurrent;
    impl Layout for AcceptCurrent {
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

        fn layout<T: Clone>(img: &ImageData<T, Self>) -> PixelStorage {
            img.layout
        }
    }

    pub(crate) trait Known: Layout {}
    impl Known for Planar {}
    impl Known for Normal {}
    impl Known for AcceptCurrent {}
}