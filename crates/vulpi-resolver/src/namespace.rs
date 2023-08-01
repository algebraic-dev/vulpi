use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_macros::Show;
use vulpi_syntax::r#abstract::Qualified;

/// An identifier for a module.
#[derive(Clone, Copy, Show, Debug, PartialEq, Eq)]
pub struct ModuleId(pub usize);

/// The visibility of a definition. It's used to distinguish between public and private definitions.
#[derive(Show, Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug)]
pub enum TypeValue {
    Enum(Qualified),
    Effect(Qualified),
    Record(Qualified),
    Abstract(Qualified),
}

impl TypeValue {
    pub fn qualified(&self) -> &Qualified {
        match self {
            TypeValue::Enum(qualified) => qualified,
            TypeValue::Effect(qualified) => qualified,
            TypeValue::Record(qualified) => qualified,
            TypeValue::Abstract(qualified) => qualified,
        }
    }
}

impl vulpi_show::Show for TypeValue {
    fn show(&self) -> vulpi_show::TreeDisplay {
        match self {
            TypeValue::Enum(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Enum: {:?}", qualified))
            }
            TypeValue::Effect(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Effect: {:?}", qualified))
            }
            TypeValue::Record(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Record: {:?}", qualified))
            }
            TypeValue::Abstract(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Abstract: {:?}", qualified))
            }
        }
    }
}

impl vulpi_show::Show for Value {
    fn show(&self) -> vulpi_show::TreeDisplay {
        match self {
            Value::Module(id) => vulpi_show::TreeDisplay::label(&format!("Module: {}", id.0)),
            Value::Function(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Function: {:?}", qualified))
            }
            Value::Effect(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Effect: {:?}", qualified))
            }
            Value::Constructor(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Constructor: {:?}", qualified))
            }
            Value::Field(qualified) => {
                vulpi_show::TreeDisplay::label(&format!("Field: {:?}", qualified))
            }
        }
    }
}

#[derive(Clone, Debug)]
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

/// A [Namespace] is a bunch of [Name] mapped to [Qualified] definitions. It's used in the first
/// step of the resolution process to map all the names to their definitions. After that, it's
/// thrown away and a [] is created.
#[derive(Show, Clone)]
pub struct Namespace {
    pub values: HashMap<Symbol, Item<Value>>,
    pub types: HashMap<Symbol, Item<TypeValue>>,
    pub modules: HashMap<Symbol, Item<ModuleId>>,
    pub pass_through: Option<ModuleId>,
}

impl Namespace {
    pub fn new(pass_through: Option<ModuleId>) -> Self {
        Self {
            values: HashMap::new(),
            types: HashMap::new(),
            modules: HashMap::new(),
            pass_through,
        }
    }
}
