use std::ops::Range;

use vulpi_location::{Byte, Spanned};
use vulpi_macros::Tree;

use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;

pub type Ident = Spanned<Symbol>;

#[derive(Debug, Clone, Tree)]
pub struct Qualified<T> {
    pub canonical: Id<id::Namespace>,
    pub last: T,
    pub range: Range<Byte>,
}

impl<T> Qualified<T> {
    pub fn new(canonical: Id<id::Namespace>, last: T, range: Range<Byte>) -> Self {
        Self {
            canonical,
            last,
            range,
        }
    }
}

impl<T: PartialEq> PartialEq for Qualified<T> {
    fn eq(&self, other: &Self) -> bool {
        self.canonical == other.canonical && self.last == other.last
    }
}

impl<T: Eq> Eq for Qualified<T> {}

impl<T: std::hash::Hash> std::hash::Hash for Qualified<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.canonical.hash(state);
        self.last.hash(state);
    }
}
