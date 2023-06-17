//! This module exposes a lot of structures that locate things inside a source code. It's really
//! useful to generate error messages.

/// A new-type for a usize. It's used to locate a byte inside a source code.
#[derive(Debug)]
pub struct Byte(usize);

/// A range of bytes inside a source code.
#[derive(Debug)]
pub struct Range<T>(T, T);

/// A span that locates a piece of data inside a source code.
#[derive(Debug)]
pub struct Spanned<T> {
    pub data: T,
    pub range: Range<Byte>,
}

impl<T> Spanned<T> {
    pub fn new(data: T, start: usize, end: usize) -> Self {
        Self {
            data,
            range: Range(Byte(start), Byte(end)),
        }
    }
}
