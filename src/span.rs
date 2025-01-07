use core::fmt;

/// offset based on the UTF-8 len.
/// A number of bytes this `char` would need if encoded in UTF-8.
type ByteOffset = usize;

#[derive(Clone)]
pub struct Span {
    offset: ByteOffset,
    len: usize,
}

impl Span {
    pub fn dummy() -> Self {
        Self {
            offset: 0_usize,
            len: 0_usize,
        }
    }

    pub fn new(offset: ByteOffset, len: usize) -> Self {
        Self { offset, len }
    }

    /// Think of `Span::len()` in terms of UTF-8 enconding lenght.
    pub fn len(&self) -> usize {
        self.len
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {}]", self.offset, self.offset + self.len())
    }
}
