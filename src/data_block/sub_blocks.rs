use std::{
    cell::RefCell,
    io::{Read, self, Take},
    rc::Rc
};

pub(super) struct SharedReader<T>(Rc<RefCell<T>>);
impl<T> Clone for SharedReader<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<T> SharedReader<T> {
    pub fn new(inner: T) -> Self {
        Self(Rc::new(RefCell::new(inner)))
    }
}
impl<R> Read for SharedReader<R> where R: Read {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut lock = self.0.try_borrow_mut()
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
        lock.read(buf)
    }
}

pub(super) struct MultiTake<R> {
    inner: SharedReader<R>,
    limits: Vec<u64>,
    index: usize,
}
impl<R> Iterator for MultiTake<R> where R: Read {
    type Item = Take<SharedReader<R>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(limit) = self.limits.get(self.index) {
            Some(self.inner.clone().take(*limit))
        } else {
            None
        }
    }
}

pub(super) trait ReadMultiTakeExt<R> {
    fn multi_take(self, limits: Vec<u64>) -> MultiTake<R>;
}

impl<R> ReadMultiTakeExt<R> for R where R: Read {
    fn multi_take(self, limits: Vec<u64>) -> MultiTake<R> {
        MultiTake {
            inner: SharedReader::new(self),
            limits,
            index: 0,
        }
    }
}

pub(super) struct MultiChainReader<I, R> {
    iter: I,
    current: Option<R>,
}
impl<I, R> MultiChainReader<I, R>
where I: Iterator<Item = io::Result<R>>,
R: Read {
    fn try_next(&mut self) -> io::Result<()> {
        self.current = self.iter.next().transpose()?;
        Ok(())
    }
}
impl<I, R> Read for MultiChainReader<I, R>
where I: Iterator<Item = io::Result<R>>,
R: Read {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if let Some(reader) = &mut self.current {
            let n = reader.read(buf)?;
            if n == 0 {
                self.try_next()?;
                self.read(buf)
            } else {
                Ok(n)
            }
        } else {
            Ok(0)
        }
    }
}

pub(super) trait MultiChain<R>: Sized {
    fn multi_chain(self) -> io::Result<MultiChainReader<Self, R>>;
}
impl<I, R> MultiChain<R> for I
where I: Iterator<Item = io::Result<R>>,
R: Read {
    fn multi_chain(self) -> io::Result<MultiChainReader<Self, R>> {
        let mut tmp = MultiChainReader::<Self, R> {
            iter: self,
            current: None,
        };
        tmp.current = tmp.iter.next().transpose()?;
        Ok(tmp)
    }
}