//! Data structures to store namespaces.

use std::collections::HashMap;

use crate::{
    id::{self, Id},
    interner::Symbol,
};

/// A single name in the source code. It's used to refer to a variable, a function, a type, etc.
/// It's basically a symbol.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name(pub Symbol);

/// A path is a sequence of names. It's used to refer to a namespace.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Path(pub Vec<Symbol>);

/// A name that is qualified by a path
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Qualified {
    pub path: Path,
    pub name: Name,
}

/// A namespace is a collection of names that can be referred to by a path.
pub struct Namespace<T> {
    pub path: Path,
    pub alias: HashMap<Path, Path>,
    pub used: HashMap<Name, Qualified>,
    names: HashMap<Name, Option<T>>,
}

impl<T> Namespace<T> {
    pub fn new(path: Path) -> Self {
        Self {
            path,
            names: HashMap::new(),
            alias: HashMap::new(),
            used: HashMap::new(),
        }
    }

    pub fn declare(&mut self, name: Name) {
        self.names.insert(name, None);
    }

    pub fn define(&mut self, name: Name, value: T) {
        self.names.insert(name, Some(value));
    }

    pub fn contains(&self, name: &Name) -> bool {
        self.names.contains_key(name)
    }

    pub fn alias(&mut self, path: Path, alias: Path) {
        self.alias.insert(path, alias);
    }

    pub fn use_as(&mut self, name: Name, path: Path) {
        self.used.insert(name.clone(), Qualified { path, name });
    }
}

/// A collection of namespaces.
pub struct Namespaces<T> {
    map_to_id: HashMap<Path, Id<id::Namespace>>,
    id_to_map: HashMap<Id<id::Namespace>, Namespace<T>>,
    counter: usize,
}

impl<T> Namespaces<T> {
    pub fn add(&mut self, path: Path, namespace: Namespace<T>) -> Id<id::Namespace> {
        let id = Id::new(self.counter);
        self.counter += 1;

        self.map_to_id.insert(path, id);
        self.id_to_map.insert(id, namespace);

        id
    }

    pub fn get(&self, id: Id<id::Namespace>) -> Option<&Namespace<T>> {
        self.id_to_map.get(&id)
    }

    pub fn get_mut(&mut self, id: Id<id::Namespace>) -> Option<&mut Namespace<T>> {
        self.id_to_map.get_mut(&id)
    }
}
