use std::marker::PhantomData;

use vulpi_storage::id::{File, Id};
use vulpi_syntax::tree::Tree;

use self::sealed::State;

pub struct Module<T: State> {
    source: Id<File>,
    parsed: Option<Tree>,
    phantom: PhantomData<T>,
}

impl Module<Loaded> {
    pub fn new(source: Id<File>) -> Self {
        Self {
            source,
            parsed: None,
            phantom: PhantomData,
        }
    }

    pub fn to_parsed(self, parsed: Tree) -> Module<Parsed> {
        Module {
            source: self.source,
            parsed: Some(parsed),
            phantom: PhantomData,
        }
    }
}

impl Module<Parsed> {
    pub fn parsed(&self) -> &Tree {
        self.parsed.as_ref().unwrap()
    }
}

pub enum Loaded {}
pub enum Resolved {}
pub enum Parsed {}

mod sealed {
    pub trait State {}
    impl State for super::Loaded {}
    impl State for super::Resolved {}
    impl State for super::Parsed {}
}
