use std::marker::PhantomData;

use vulpi_storage::id::{File, Id};

use self::sealed::State;

pub struct Module<T: State> {
    _source: Id<File>,
    phantom: PhantomData<T>,
}

impl Module<Loaded> {
    pub fn new(_source: Id<File>) -> Self {
        Self {
            _source,
            phantom: PhantomData,
        }
    }
}

pub enum Loaded {}
pub enum Resolved {}

mod sealed {
    pub trait State {}
    impl State for super::Loaded {}
    impl State for super::Resolved {}
}
