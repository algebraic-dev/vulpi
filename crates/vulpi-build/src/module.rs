use std::marker::PhantomData;

use vulpi_storage::id::{File, Id};

use self::sealed::State;

pub struct Module<T: State> {
    source: Id<File>,
    phantom: PhantomData<T>,
}

impl Module<Loaded> {
    pub fn new(source: Id<File>) -> Self {
        Self {
            source,
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
