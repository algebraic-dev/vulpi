//! Unique identifier for a bunch of structures. It's used to identify a type, a function, a
//! variable and other useful things. The main structure of this module is the [Id]

use std::marker::PhantomData;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id<T: Identifier>(usize, PhantomData<T>);

impl<T: Identifier> Id<T> {
    pub(crate) fn new(id: usize) -> Self {
        Self(id, PhantomData)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Namespace {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum File {}

mod sealed {
    pub trait Identifier {}

    impl Identifier for super::Type {}

    impl Identifier for super::Namespace {}

    impl Identifier for super::File {}
}

pub trait Identifier: sealed::Identifier {}

impl<T: sealed::Identifier> Identifier for T {}
