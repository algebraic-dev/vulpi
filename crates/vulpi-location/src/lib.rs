//! This module exposes a lot of structures that locate things inside a source code. It's really
//! useful to generate error messages.

use std::ops::Range;

use vulpi_show::{Show, TreeDisplay};
use vulpi_storage::id::{File, Id};

/// A new-type for a usize. It's used to locate a byte inside a source code.
#[derive(Debug, Clone)]
pub struct Byte(pub usize);

/// A span that locates a piece of data inside a source code.
#[derive(Clone)]
pub struct Spanned<T> {
    pub data: T,
    pub range: Range<Byte>,
}

impl<T: Show> Show for Spanned<T> {
    fn show(&self) -> TreeDisplay {
        self.data.show()
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Spanned").field(&self.data).finish()
    }
}

impl<T> Spanned<T> {
    pub fn new(data: T, start: usize, end: usize) -> Self {
        Self {
            data,
            range: Byte(start)..Byte(end),
        }
    }
}

#[derive(Clone)]
pub struct Location {
    pub file: Id<File>,
    pub range: Range<Byte>,
}

impl Location {
    pub fn new(file: Id<File>, range: Range<Byte>) -> Self {
        Self { file, range }
    }
}
