//! Unique identifier for a bunch of structures. It's used to identify a type, a function, a
//! variable and other useful things. The main structure of this module is the [Id]

use std::marker::PhantomData;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id<T>(usize, PhantomData<T>);

impl<T> Id<T> {
    pub(crate) fn new(id: usize) -> Self {
        Self(id, PhantomData)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Module {}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum File {}

mod sealed {
    pub trait Identifier {}

    impl Identifier for super::Type {}

    impl Identifier for super::Module {}

    impl Identifier for super::File {}
}
