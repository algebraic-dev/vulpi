pub mod no_rc;

use std::marker::PhantomData;

pub use no_rc::*;
use vulpi_show::{Show, TreeDisplay};

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

impl<T> Show for Interned<T> {
    fn show(&self) -> vulpi_show::TreeDisplay {
        TreeDisplay::label(&self.0.get())
    }
}

impl<T> std::fmt::Debug for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Interned")
            .field(&self.0)
            .field(&self.1)
            .finish()
    }
}
