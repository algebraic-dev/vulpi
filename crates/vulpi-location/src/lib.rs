//! This module exposes a lot of structures that locate things inside a source code. It's really
//! useful to generate error messages.

use std::ops::Range;

use vulpi_storage::id::{File, Id};

/// A new-type for a usize. It's used to locate a byte inside a source code.
#[derive(Debug, Clone)]
pub struct Byte(usize);

/// A span that locates a piece of data inside a source code.
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub data: T,
    pub range: Range<Byte>,
}

impl<T> Spanned<T> {
    pub fn new(data: T, start: usize, end: usize) -> Self {
        Self {
            data,
            range: Byte(start)..Byte(end),
        }
    }
}

pub struct Location {
    pub file: Id<File>,
    pub range: Range<Byte>,
}
