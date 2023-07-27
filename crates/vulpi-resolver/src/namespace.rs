use std::{collections::HashMap, fmt::Display};

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_macros::Show;

/// An identifier for a module.
#[derive(Clone, Copy, Show, Debug)]
pub struct ModuleId(pub usize);

/// The visibility of a definition. It's used to distinguish between public and private definitions.
#[derive(Show, Clone, Debug)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Clone)]
pub enum Value {
    Module(ModuleId),
    Function(Qualified),
    Effect(Qualified),
    Constructor(Qualified),
    Field(Qualified),
}

impl vulpi_show::Show for Value {
    fn show(&self) -> vulpi_show::TreeDisplay {
        match self {
            Value::Module(id) => vulpi_show::TreeDisplay::label(&format!("Module: {}", id.0)),
            Value::Function(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Function: {}", qualified))
            }
            Value::Effect(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Effect: {}", qualified))
            }
            Value::Constructor(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Constructor: {}", qualified))
            }
            Value::Field(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Field: {}", qualified))
            }
        }
    }
}

#[derive(Clone)]
pub struct Item<Kind> {
    pub visibility: Visibility,
    pub span: Span,
    pub item: Kind,
}

impl<T: vulpi_show::Show> vulpi_show::Show for Item<T> {
    fn show(&self) -> vulpi_show::TreeDisplay {
        vulpi_show::TreeDisplay::label(&format!("{:?} ", self.visibility)).with(self.item.show())
    }
}

#[derive(Clone, Debug)]
pub struct Qualified {
    pub path: ModuleId,
    pub name: Symbol,
}

impl Display for Qualified {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.path.0, self.name.get())
    }
}

impl vulpi_show::Show for Qualified {
    fn show(&self) -> vulpi_show::TreeDisplay {
        vulpi_show::TreeDisplay::label(&format!("Qualified: {}", self))
    }
}

/// A [Namespace] is a bunch of [Name] mapped to [Qualified] definitions. It's used in the first
/// step of the resolution process to map all the names to their definitions. After that, it's
/// thrown away and a [] is created.
#[derive(Default, Show, Clone)]
pub struct Namespace {
    pub values: HashMap<Symbol, Item<Value>>,
    pub types: HashMap<Symbol, Item<Qualified>>,
}
