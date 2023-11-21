//! Module for declaration of top level items inside the type checker. The main structure of this
//! module is the [Module] structure that is responsible for storing the types of the top level
//! items.

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract::Qualified;

use crate::{r#virtual::Virtual, real::Real, Type};

#[derive(Clone)]
pub enum Def {
    Enum(Vec<Qualified>),
    Record(Vec<Qualified>),
    Effect(Vec<Qualified>),
    Type,
    Constraint
}

#[derive(Clone)]
pub struct TypeData {
    pub kind: Type<Virtual>,
    pub binders: Vec<(Symbol, Type<Virtual>)>,
    pub module: Symbol,
    pub def: Def,
}

#[derive(Clone)]
pub struct TraitData {
    pub kind: Type<Virtual>,
    pub binders: Vec<Type<Virtual>>,
    pub supers: Vec<Type<Real>>,
    pub signatures: Vec<(Qualified, Type<Real>)>,


}

#[derive(Clone)]
pub struct LetDef {
    pub typ: Type<Virtual>,
    pub unbound: Vec<(Symbol, Type<Real>)>,
    pub args: Vec<Type<Real>>,
    pub ret: Type<Virtual>,
}

#[derive(Default)]
pub struct Interface {
    /// The types of the functions.
    pub variables: HashMap<Symbol, LetDef>,

    /// The types of the functions.
    pub constructors: HashMap<Symbol, (Type<Real>, usize, Qualified)>,

    /// The types of the types.
    pub types: HashMap<Symbol, TypeData>,

    /// The fields of the records.
    pub fields: HashMap<Symbol, Type<Real>>,

    /// Traits.
    pub traits: HashMap<Symbol, TraitData>,
}

#[derive(Default)]
pub struct Modules {
    /// The modules.
    pub modules: HashMap<Symbol, Interface>,
}

impl Modules {
    pub fn new() -> Self {
        Self {
            modules: Default::default(),
        }
    }

    pub fn typ(&mut self, qualified: &Qualified) -> TypeData {
        let module = self.get(&qualified.path);
        module.types.get(&qualified.name).unwrap().clone()
    }

    pub fn constructor(&mut self, qualified: &Qualified) -> (Type<Real>, usize, Qualified) {
        let module = self.get(&qualified.path);
        module.constructors.get(&qualified.name).unwrap().clone()
    }

    pub fn let_decl(&mut self, qualified: &Qualified) -> &mut LetDef {
        let module = self.get(&qualified.path);
        module.variables.get_mut(&qualified.name).unwrap()
    }

    pub fn field(&mut self, qualified: &Qualified) -> Type<Real> {
        let module = self.get(&qualified.path);
        module.fields.get(&qualified.name).unwrap().clone()
    }

    pub fn get(&mut self, id: &Symbol) -> &mut Interface {
        self.modules.entry(id.clone()).or_default()
    }
}
