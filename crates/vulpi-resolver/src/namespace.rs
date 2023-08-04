//! This module contains the definition of the [Namespace] struct and all the types that are used
//! to represent the definitions inside of it.

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_macros::Show;
use vulpi_show::TreeDisplay;
use vulpi_syntax::r#abstract::Qualified;

use crate::{module_tree::Tree, paths};

/// An identifier for a module.
pub type ModuleId = Symbol;

/// The visibility of a definition. It's used to distinguish between public and private definitions.
#[derive(Show, Clone, Debug, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

/// A [Value] is a definition in a [Namespace] that is in the value namespace.
#[derive(Clone)]
pub enum Value {
    Module(ModuleId),
    Function(Qualified),
    Effect(Qualified),
    Constructor(Qualified),
    Field(Qualified),
}

/// A [TypeValue] is a definition in a [Namespace] that is in the type namespace.
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
    fn show(&self) -> TreeDisplay {
        match self {
            TypeValue::Enum(q) => TreeDisplay::label(&format!("Enum: {:?}", q)),
            TypeValue::Effect(q) => TreeDisplay::label(&format!("Effect: {:?}", q)),
            TypeValue::Record(q) => TreeDisplay::label(&format!("Record: {:?}", q)),
            TypeValue::Abstract(q) => TreeDisplay::label(&format!("Abstract: {:?}", q)),
        }
    }
}

impl vulpi_show::Show for Value {
    fn show(&self) -> TreeDisplay {
        match self {
            Value::Module(id) => TreeDisplay::label(&format!("Module: {}", id.get())),
            Value::Function(q) => TreeDisplay::label(&format!("Function: {:?}", q)),
            Value::Effect(q) => TreeDisplay::label(&format!("Effect: {:?}", q)),
            Value::Constructor(q) => TreeDisplay::label(&format!("Constructor: {:?}", q)),
            Value::Field(q) => TreeDisplay::label(&format!("Field: {:?}", q)),
        }
    }
}

/// An [Item] is a definition in a [Namespace]. It's used to map a [Name] to a [Qualified] definition.
#[derive(Clone, Debug)]
pub struct Item<Kind> {
    pub parent: Option<ModuleId>,
    pub visibility: Visibility,
    pub span: Span,
    pub item: Kind,
}

impl<T: vulpi_show::Show> vulpi_show::Show for Item<T> {
    fn show(&self) -> TreeDisplay {
        TreeDisplay::label(&format!("{:?} ", self.visibility)).with(self.item.show())
    }
}

/// A [Namespace] is a bunch of [Name] mapped to [Qualified] definitions. It's used in the first
/// step of the resolution process to map all the names to their definitions. After that, it's
/// thrown away and a [] is created.
#[derive(Show, Clone)]
pub struct Namespace {
    pub name: Symbol,
    pub values: HashMap<Symbol, Item<Value>>,
    pub types: HashMap<Symbol, Item<TypeValue>>,
    pub modules: HashMap<Symbol, Item<ModuleId>>,
}

impl Namespace {
    pub fn new(name: Symbol) -> Self {
        Self {
            name,
            values: HashMap::new(),
            types: HashMap::new(),
            modules: HashMap::new(),
        }
    }
}

impl Namespace {
    pub fn merge(&mut self, other: Namespace) {
        self.values.extend(other.values);
        self.types.extend(other.types);
        self.modules.extend(other.modules);
    }
}

pub enum Resolve {
    ModuleNotFound(Symbol),
    ModuleFound(Symbol),
    PrivateModule(Symbol),
}

pub struct Namespaces {
    pub tree: Tree,
    pub namespaces: HashMap<Symbol, Namespace>,
}

impl Namespaces {
    pub fn get(&self, name: paths::Path) -> Option<&Namespace> {
        self.namespaces.get(&name.symbol())
    }

    pub fn get_mut(&mut self, name: paths::Path) -> Option<&mut Namespace> {
        self.namespaces.get_mut(&name.symbol())
    }

    pub fn find(&self, name: Symbol) -> Option<&Namespace> {
        self.namespaces.get(&name)
    }

    pub fn add(&mut self, name: paths::Path) -> Option<&mut Tree> {
        let symbol = name.symbol();

        self.namespaces
            .entry(symbol)
            .or_insert_with(|| Namespace::new(symbol));

        self.tree.add(name.slice(), symbol)
    }

    pub fn resolve(&mut self, current: paths::Path, mut name: paths::Path) -> Resolve {
        let module = self.tree.find(current.slice()).unwrap();
        let mut id = module.id;
        let mut namespace = self.namespaces.get(&module.id).unwrap();

        if let Some((head, tail)) = name.split_first() {
            if head.get() == "Self" {
                namespace = self.namespaces.get(&self.tree.id).unwrap();
                name = tail;
            }
        }

        while let Some((head, tail)) = name.split_first() {
            name = tail;
            if let Some(item) = namespace.modules.get(&head) {
                if item.visibility == Visibility::Private && Some(module.id) != item.parent {
                    return Resolve::PrivateModule(head);
                } else {
                    namespace = self.namespaces.get(&item.item).unwrap();
                    id = item.item;
                }
            } else {
                return Resolve::ModuleNotFound(head);
            }
        }

        Resolve::ModuleFound(namespace.name)
    }
}
