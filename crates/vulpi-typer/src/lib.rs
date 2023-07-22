use std::collections::HashMap;
use types::{Kind, Scheme};
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;

pub mod check;
pub mod context;
pub mod declare;
pub mod error;
pub mod infer;
pub mod types;
pub mod unify;

#[derive(PartialEq, Eq, Hash)]
pub enum Data {
    Let(Symbol),
    Constructor(Symbol),
    Field(Symbol),
}

#[derive(Default)]
pub struct Interface {
    pub types: HashMap<Symbol, Kind>,

    pub lets: HashMap<Symbol, Scheme>,
    pub cons: HashMap<Symbol, Scheme>,
    pub fields: HashMap<Symbol, Scheme>,
}

#[derive(Default)]
pub struct Modules {
    pub modules: HashMap<Id<id::Namespace>, Interface>,
}

impl Modules {
    pub fn declare_type(&mut self, id: Id<id::Namespace>, name: Symbol, kind: Kind) {
        self.modules.entry(id).or_default().types.insert(name, kind);
    }

    pub fn declare_let(&mut self, id: Id<id::Namespace>, name: Symbol, scheme: Scheme) {
        self.modules
            .entry(id)
            .or_default()
            .lets
            .insert(name, scheme);
    }

    pub fn declare_cons(&mut self, id: Id<id::Namespace>, name: Symbol, scheme: Scheme) {
        self.modules
            .entry(id)
            .or_default()
            .cons
            .insert(name, scheme);
    }

    pub fn declare_field(&mut self, id: Id<id::Namespace>, name: Symbol, scheme: Scheme) {
        self.modules
            .entry(id)
            .or_default()
            .fields
            .insert(name, scheme);
    }
}
