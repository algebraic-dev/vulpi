use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_macros::Show;

/// An identifier for a module.
#[derive(Clone, Copy, Show)]
pub struct ModuleId(pub usize);

/// The visibility of a definition. It's used to distinguish between public and private definitions.
#[derive(Show)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Show)]
pub enum Value {
    Module(ModuleId),
    Function(Qualified),
    Effect(Qualified),
    Constructor(Qualified),
    Field(Qualified),
}

#[derive(Show)]
pub struct Item<Kind> {
    pub visibility: Visibility,
    pub item: Kind,
}

#[derive(Show)]
pub struct Qualified {
    pub path: ModuleId,
    pub name: Symbol,
}

/// A [Namespace] is a bunch of [Name] mapped to [Qualified] definitions. It's used in the first
/// step of the resolution process to map all the names to their definitions. After that, it's
/// thrown away and a [] is created.
#[derive(Default, Show)]
pub struct Namespace {
    pub values: HashMap<Symbol, Item<Value>>,
    pub types: HashMap<Symbol, Item<Qualified>>,
}
