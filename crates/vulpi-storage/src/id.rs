//! Unique identifier for a bunch of structures. It's used to identify a type, a function, a
//! variable and other useful things. The main structure of this module is the [Id]

use std::marker::PhantomData;

use petgraph::stable_graph::{self, IndexType};
use vulpi_macros::Tree;
use vulpi_tree::{Show, TreeDisplay};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Id<T: Identifier>(usize, PhantomData<T>);

impl<T: Identifier> Id<T> {
    pub fn index(&self) -> usize {
        self.0
    }
}

impl<T: Identifier> Show for Id<T> {
    fn show(&self) -> vulpi_tree::TreeDisplay {
        TreeDisplay::label(&format!("{}:{}", std::any::type_name::<T>(), self.0))
    }
}

impl<
        T: Identifier
            + std::fmt::Debug
            + std::cmp::Ord
            + std::hash::Hash
            + std::default::Default
            + Copy
            + 'static,
    > From<stable_graph::NodeIndex<Id<T>>> for Id<T>
{
    fn from(val: stable_graph::NodeIndex<Id<T>>) -> Self {
        Id(val.index(), PhantomData)
    }
}

unsafe impl<
        T: Identifier
            + std::fmt::Debug
            + std::cmp::Ord
            + std::hash::Hash
            + std::default::Default
            + Copy
            + 'static,
    > IndexType for Id<T>
{
    fn new(x: usize) -> Self {
        Self::new(x)
    }

    fn index(&self) -> usize {
        self.0
    }

    fn max() -> Self {
        Self::new(usize::max_value())
    }
}

impl<T: Identifier> Id<T> {
    pub fn new(id: usize) -> Self {
        Self(id, PhantomData)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Tree)]
pub struct Type;
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Tree)]
pub struct Namespace;
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Tree)]
pub struct File;

mod sealed {
    pub trait Identifier {}

    impl Identifier for super::Type {}

    impl Identifier for super::Namespace {}

    impl Identifier for super::File {}
}

pub trait Identifier: sealed::Identifier {}

impl<T: sealed::Identifier> Identifier for T {}
