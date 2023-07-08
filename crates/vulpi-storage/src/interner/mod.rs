pub mod no_rc;

use std::marker::PhantomData;

pub use no_rc::*;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Interned<T>(Symbol, PhantomData<T>);

impl<T> Interned<T> {
    pub fn new(symbol: Symbol) -> Self {
        Self(symbol, PhantomData)
    }

    pub fn get(&self) -> String {
        self.0.get()
    }
}

pub trait Internable: Sized {
    fn intern(&self) -> Interned<Self>;
}
