//! Structure for handling ambiguity in the resolver.

use std::collections::{HashMap, HashSet};

use vulpi_show::TreeDisplay;
use vulpi_storage::interner::Symbol;

pub type Path = Vec<Symbol>;

/// This is a structure that represents if a name is ambiguous or not inside the module. It's useful
/// to report errors lately when something is used.
pub enum Ambiguity<T> {
    Single(T),
    Ambiguous(HashSet<T>),
}

impl<T: std::hash::Hash + PartialEq + Eq> Ambiguity<T> {
    pub fn new(key: T) -> Self {
        Self::Single(key)
    }

    pub fn to_ambiguous(self) -> Self {
        match self {
            Self::Single(key) => Self::Ambiguous(std::iter::once(key).collect()),
            Self::Ambiguous(map) => Self::Ambiguous(map),
        }
    }

    pub fn add(&mut self, key: T) {
        match self {
            Self::Single(k) if *k != key => {
                let empty = unsafe { std::mem::zeroed() };
                let res = std::mem::replace(self, empty);
                match res {
                    Self::Single(k) => {
                        let mut val = HashSet::new();
                        val.insert(k);
                        val.insert(key);
                        *self = Self::Ambiguous(val)
                    }
                    _ => unreachable!(),
                }
            }
            Self::Ambiguous(ref mut map) => {
                map.insert(key);
            }
            _ => (),
        }
    }

    pub fn is_ambiguous(&self) -> bool {
        matches!(self, Self::Ambiguous(_))
    }

    pub fn get_canonical(&self) -> &T {
        match self {
            Self::Single(res) => res,
            Self::Ambiguous(map) => map.iter().next().unwrap(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum DataType {
    Type(Symbol),
    Let(Symbol),
    Constructor(Symbol),
}

impl DataType {
    pub fn symbol(&self) -> &Symbol {
        match self {
            Self::Type(symbol) => symbol,
            Self::Let(symbol) => symbol,
            Self::Constructor(symbol) => symbol,
        }
    }
}

impl vulpi_show::Show for DataType {
    fn show(&self) -> TreeDisplay {
        match self {
            Self::Type(name) => TreeDisplay::label("Type").with(name.show()),
            Self::Let(name) => TreeDisplay::label("Let").with(name.show()),
            Self::Constructor(name) => TreeDisplay::label("Constructor").with(name.show()),
        }
    }
}

/// A map that represents imports in the current scope.
#[derive(Default)]
pub struct ImportMap<U> {
    map: HashMap<DataType, Ambiguity<U>>,
}

impl<U: std::hash::Hash + PartialEq + Eq> ImportMap<U> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn add(&mut self, key: DataType, range: U) {
        match self.map.get_mut(&key) {
            Some(ambiguity) => ambiguity.add(range),
            None => {
                self.map.insert(key.clone(), Ambiguity::new(range));
            }
        }
    }

    pub fn get(&self, key: &DataType) -> Option<&Ambiguity<U>> {
        self.map.get(key)
    }
}
