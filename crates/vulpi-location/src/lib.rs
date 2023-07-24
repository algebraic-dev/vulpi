//! This module exposes a lot of structures that locate things inside a source code. It's really
//! useful to generate error messages.

use std::fmt::Debug;

/// A new-type for a usize. It's used to locate a byte inside a source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Byte(pub usize);

/// A span that locates a piece of data inside a source code.
#[derive(Clone)]
pub struct Span {
    pub file: FileId,
    pub start: Byte,
    pub end: Byte,
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}~{:?}", self.start.0, self.end.0)
    }
}

impl Span {
    pub fn new(file: FileId, start: Byte, end: Byte) -> Self {
        Self { file, start, end }
    }

    pub fn from_usize(file: FileId, start: usize, end: usize) -> Self {
        Self {
            file,
            start: Byte(start),
            end: Byte(end),
        }
    }

    pub fn mix(self, other: Self) -> Self {
        Self {
            file: self.file,
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
        }
    }
}

/// A span that locates a piece of data inside a source code.
#[derive(Clone)]
pub struct Spanned<T> {
    pub data: T,
    pub range: Span,
}

impl<T> Spanned<T> {
    pub fn map<U>(&self, f: impl FnOnce(&T) -> U) -> Spanned<U> {
        Spanned {
            data: f(&self.data),
            range: self.range.clone(),
        }
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Spanned").field(&self.data).finish()
    }
}

impl<T> Spanned<T> {
    pub fn new(data: T, range: Span) -> Self {
        Self { data, range }
    }
}

/// The identifier of a file.
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FileId(pub usize);
