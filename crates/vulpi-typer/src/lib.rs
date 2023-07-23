use std::collections::HashMap;
use types::{Kind, Scheme, Type};
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;
use vulpi_syntax::resolved::Qualified;

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
    pub types: HashMap<Symbol, TypeRep>,
    pub lets: HashMap<Symbol, LetDef>,
    pub cons: HashMap<Symbol, ConsDef>,
    pub fields: HashMap<Symbol, Scheme>,
}

#[derive(Clone)]
pub struct LetDef {
    pub args: Vec<Type>,
    pub params: Vec<Symbol>,
    pub ret: Type,
    pub typ: Scheme,
}

#[derive(Clone)]
pub struct ConsDef {
    pub arity: usize,
    pub typ: Scheme,
}

#[derive(Clone)]
pub enum Def {
    Record(Vec<Qualified>),
    Enum,
}

#[derive(Clone)]
pub struct TypeRep {
    pub kind: Kind,
    pub def: Def,
    pub params: Vec<Symbol>,
}

#[derive(Default)]
pub struct Modules {
    pub modules: HashMap<Id<id::Namespace>, Interface>,
}

impl Modules {
    pub fn declare_type(&mut self, id: Id<id::Namespace>, name: Symbol, kind: TypeRep) {
        self.modules.entry(id).or_default().types.insert(name, kind);
    }

    pub fn declare_let(&mut self, id: Id<id::Namespace>, name: Symbol, scheme: LetDef) {
        self.modules
            .entry(id)
            .or_default()
            .lets
            .insert(name, scheme);
    }

    pub fn declare_cons(&mut self, id: Id<id::Namespace>, name: Symbol, scheme: ConsDef) {
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

    pub fn get_let(&self, id: Id<id::Namespace>, name: &Symbol) -> Option<&LetDef> {
        self.modules.get(&id).and_then(|x| x.lets.get(name))
    }

    pub fn get_cons(&self, id: Id<id::Namespace>, name: &Symbol) -> Option<&ConsDef> {
        self.modules.get(&id).and_then(|x| x.cons.get(name))
    }

    pub fn get_type(&self, id: Id<id::Namespace>, name: &Symbol) -> Option<&TypeRep> {
        self.modules.get(&id).and_then(|x| x.types.get(name))
    }

    pub fn get_field(&self, id: Id<id::Namespace>, name: &Symbol) -> Option<&Scheme> {
        self.modules.get(&id).and_then(|x| x.fields.get(name))
    }
}
