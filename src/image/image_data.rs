use ndarray::ArrayD;
use num_complex::Complex;
use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut}
};
use super::PixelStorage;

pub type RawImageData<T> = ImageData<T, Any>;

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

pub(crate) trait IntoDynImageData {
    fn into_dyn_img(self, layout: PixelStorage) -> DynImageData;
}
macro_rules! into_impl {
    ($enum:ident, $t:ty) => {
        impl IntoDynImageData for ArrayD<$t> {
            fn into_dyn_img(self, layout: PixelStorage) -> DynImageData {
                DynImageData::$enum(ImageData::<$t, Any>::new(self, layout))
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

use data_layout::*;

#[derive(Clone, Debug)]
pub struct ImageData<T: Clone, L: Layout> {
    inner: ArrayD<T>,
    /// Uses an associated type to save some memory once the layout is known.
    /// When the data is first read and `L` is [`Any`], `layout` will store a [`PixelStorage`] value.
    /// After the data has been transformed into a desired memory layout, `layout` will just be [`PhantomData`]
    /// since the layout is encoded in the template parameter, and storing it here would be redundant.
    layout: L::Storage,
}
impl<T: Clone> ImageData<T, Any> {
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
impl<T: Clone, L: Layout> ImageData<T, L> {
    pub fn into_planar_layout(self) -> ImageData<T, Planar> {
        L::into_planar(self)
    }
    pub fn into_normal_layout(self) -> ImageData<T, Normal> {
        L::into_normal(self)
    }
    pub fn layout(&self) -> PixelStorage {
        L::layout(&self)
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

pub mod data_layout {
    use super::*;
    pub trait Layout: Sized {
        type Storage;
        fn into_planar<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Planar>;
        fn into_normal<T: Clone>(img: ImageData<T, Self>) -> ImageData<T, Normal>;
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
        fn layout<T: Clone>(_img: &ImageData<T, Self>) -> PixelStorage {
            PixelStorage::Normal
        }
    }

    #[derive(Clone, Debug)]
    pub struct Any;
    impl Layout for Any {
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

        #[inline]
        fn layout<T: Clone>(img: &ImageData<T, Self>) -> PixelStorage {
            img.layout
        }
    }

    /// The exact same thing as [`Any`] except [`ImageData<T, AcceptCurrent>`] implements [`Deref`], meaning its inner data can be accessed
    #[derive(Debug, Clone)]
    pub struct AcceptCurrent;
    impl Layout for AcceptCurrent {
        type Storage = <Any as Layout>::Storage;

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

        fn layout<T: Clone>(img: &ImageData<T, Self>) -> PixelStorage {
            img.layout
        }
    }

    pub(crate) trait Known: Layout {}
    impl Known for Planar {}
    impl Known for Normal {}
    impl Known for AcceptCurrent {}
}