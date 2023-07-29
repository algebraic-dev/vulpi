//! The string interner of the compiler. It is used to store strings in a way that is more efficient
//! than storing them directly. It is also used to make sure that strings are unique. This is
//! important for the compiler because it allows us to compare strings by comparing their ids. None
//! of the implementations implement the [Copy] trait because some of them are heavy to clone.

#[cfg(feature = "single-shot")]
pub mod no_rc;

#[cfg(feature = "single-shot")]
pub use no_rc::*;

use std::marker::PhantomData;

/// A interned symbol that contains a phantom data to make it unique.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Interned<T>(Symbol, PhantomData<T>);

impl<T> Interned<T> {
    pub fn new(symbol: Symbol) -> Self {
        Self(symbol, PhantomData)
    }

    pub fn get(&self) -> String {
        self.0.get()
    }

    pub fn cast<U>(self) -> Interned<U> {
        Interned::new(self.0)
    }
}

pub trait Internable: Sized {
    fn intern(&self) -> Interned<Self>;
}

impl<T> std::fmt::Debug for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Interned")
            .field(&self.0)
            .field(&self.1)
            .finish()
    }
}
