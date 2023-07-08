//! Data structures to store namespaces.

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    path::PathBuf,
};

use crate::{
    id::{self, Id},
    interner::{Internable, Interned, Symbol},
};

/// A single name in the source code. It's used to refer to a variable, a function, a type, etc.
/// It's basically a symbol.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name(pub Symbol);

impl Internable for Name {
    fn intern(&self) -> Interned<Self> {
        Interned::new(Symbol::intern(&self.0.get()))
    }
}

/// A path is a sequence of names. It's used to refer to a namespace.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Path(pub Vec<Symbol>);

impl Internable for Path {
    fn intern(&self) -> Interned<Self> {
        Interned::new(Symbol::intern(&self.to_string()))
    }
}

impl Path {
    pub fn to_path(&self, extension: &str) -> PathBuf {
        let mut path = PathBuf::new();
        for symbol in &self.0 {
            path.push(symbol.get());
        }

        path.set_extension(extension);

        path
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first.get())?;
            for symbol in iter {
                write!(f, ".{}", symbol.get())?;
            }
        }

        Ok(())
    }
}

impl Path {
    pub fn join(self, other: Path) -> Path {
        Path(self.0.into_iter().chain(other.0.into_iter()).collect())
    }
}

impl From<&[&str]> for Path {
    fn from(value: &[&str]) -> Self {
        Path(value.iter().map(|x| Symbol::intern(x)).collect())
    }
}

impl From<&[Symbol]> for Path {
    fn from(value: &[Symbol]) -> Self {
        Path(value.to_vec())
    }
}

/// A name that is qualified by a path
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Qualified {
    pub path: Path,
    pub name: Name,
}

impl Qualified {
    pub fn new(path: Path, name: Name) -> Self {
        Self { path, name }
    }
}

impl Internable for Qualified {
    fn intern(&self) -> Interned<Self> {
        Interned::new(Symbol::intern(&self.to_string()))
    }
}

impl Display for Qualified {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.path.0.is_empty() {
            write!(f, "{}", self.name.0.get())
        } else {
            write!(f, "{}.{}", self.path, self.name.0.get())
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct Leveled<T> {
    pub types: T,
    pub values: T,
}

impl<T> Leveled<T> {
    pub fn map<U>(self, f: impl Fn(T) -> U) -> Leveled<U> {
        Leveled {
            types: f(self.types),
            values: f(self.values),
        }
    }
}

/// Tracks how many times a name is used with different paths.
#[derive(Clone, Debug)]
pub struct Uses {
    data: HashSet<Qualified>,
}

impl Uses {
    pub fn new(qualified: Qualified) -> Self {
        let mut data = HashSet::new();
        data.insert(qualified);

        Self { data }
    }

    pub fn add(&mut self, qualified: Qualified) {
        self.data.insert(qualified);
    }

    pub fn empty() -> Self {
        Self {
            data: HashSet::new(),
        }
    }

    pub fn ambiguous(&self) -> bool {
        self.data.len() > 1
    }

    pub fn merge(&mut self, other: Self) {
        self.data.extend(other.data);
    }
}

#[derive(Clone, Debug)]
pub struct OccMap<K> {
    data: HashMap<K, Uses>,
}

impl<K: Eq + std::hash::Hash> FromIterator<(K, Qualified)> for OccMap<K> {
    fn from_iter<T: IntoIterator<Item = (K, Qualified)>>(iter: T) -> Self {
        let mut data = HashMap::new();

        for (name, qualified) in iter {
            data.entry(name).or_insert_with(Uses::empty).add(qualified);
        }

        Self { data }
    }
}

impl<K> Default for OccMap<K> {
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

impl<K: std::hash::Hash + PartialEq + Eq> OccMap<K> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    pub fn get(&self, name: &K) -> Option<&Uses> {
        self.data.get(name)
    }

    pub fn add(&mut self, name: K, qualified: Qualified) {
        self.data
            .entry(name)
            .or_insert_with(Uses::empty)
            .add(qualified);
    }

    pub fn extend(&mut self, other: Self) {
        for (name, uses) in other.data {
            self.data
                .entry(name)
                .or_insert_with(Uses::empty)
                .merge(uses);
        }
    }
}

/// A namespace is a collection of names that can be referred to by a path.
#[derive(Clone, Debug)]
pub struct Namespace<T> {
    pub path: Path,

    /// File that it was declared in.
    pub file_id: Id<id::File>,

    // Import aliases that happen on `import A as B`
    pub aliases: HashMap<Path, Path>,

    // Names that are used on the both levels.
    pub used: Leveled<OccMap<Name>>,

    /// Names are declared on both levels but they are not defined on the first level.
    pub declared: Leveled<HashMap<Name, Option<T>>>,
}

impl<T: Clone> Namespace<T> {
    pub fn new(path: Path, file_id: Id<id::File>) -> Self {
        Self {
            path,
            file_id,
            aliases: HashMap::new(),
            used: Default::default(),
            declared: Default::default(),
        }
    }

    pub fn derive(&self, path: Path) -> Self {
        let mut namespace = self.clone();
        namespace.path = path;
        namespace
    }

    pub fn add_used_type(&mut self, name: Qualified) {
        self.used.types.add(name.name.clone(), name);
    }

    pub fn add_used_value(&mut self, name: Qualified) {
        self.used.values.add(name.name.clone(), name);
    }

    pub fn declare_type(&mut self, name: Name) {
        self.declared.types.insert(name, None);
    }

    pub fn declare_value(&mut self, name: Name) {
        self.declared.values.insert(name, None);
    }

    pub fn define_type(&mut self, name: Name, value: T) {
        self.declared.types.insert(name, Some(value));
    }

    pub fn define_value(&mut self, name: Name, value: T) {
        self.declared.values.insert(name, Some(value));
    }

    pub fn contains_type(&self, name: &Name) -> bool {
        self.declared.types.contains_key(name)
    }

    pub fn contains_value(&self, name: &Name) -> bool {
        self.declared.values.contains_key(name)
    }

    pub fn alias(&mut self, path: Path, alias: Path) {
        self.aliases.insert(path, alias);
    }

    pub fn merge(&mut self, declared: Leveled<OccMap<Name>>) {
        self.used.types.extend(declared.types.clone());
        self.used.values.extend(declared.values);
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

    pub fn get_id(&self, path: &Path) -> Option<Id<id::Namespace>> {
        self.map_to_id.get(path).copied()
    }

    pub fn get(&self, id: Id<id::Namespace>) -> Option<&Namespace<T>> {
        self.id_to_map.get(&id)
    }

    pub fn get_by_path(&self, path: &Path) -> Option<&Namespace<T>> {
        self.map_to_id
            .get(path)
            .and_then(|id| self.id_to_map.get(id))
    }

    pub fn get_mut(&mut self, id: Id<id::Namespace>) -> Option<&mut Namespace<T>> {
        self.id_to_map.get_mut(&id)
    }
}

impl<T> Default for Namespaces<T> {
    fn default() -> Self {
        Namespaces {
            map_to_id: HashMap::new(),
            id_to_map: HashMap::new(),
            counter: 0,
        }
    }
}
