//! Module for declaration of top level items inside the type checker. The main structure of this
//! module is the [Module] structure that is responsible for storing the types of the top level
//! items.

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::{elaborated, r#abstract::Qualified};

use crate::{
    r#type::{r#virtual::Virtual, Effect, Type},
    Real,
};

#[derive(Clone)]
pub enum Def {
    Enum(Vec<Qualified>),
    Record(Vec<Qualified>),
    Effect(Vec<Qualified>),
    Type,
}

#[derive(Clone)]
pub struct TypeData {
    pub kind: Type<Virtual>,
    pub binders: Vec<(Symbol, Type<Virtual>)>,
    pub module: Symbol,
    pub def: Def,
}

pub struct LetDef {
    pub typ: Type<Virtual>,
    pub binders: HashMap<Symbol, Type<Real>>,
    pub unbound: Vec<(Symbol, Type<Real>)>,
    pub ambient: Effect<Real>,
    pub ret: Type<Virtual>,
    pub unbound_effects: Vec<(Symbol, Type<Real>)>,
    pub elab_binders: Vec<(elaborated::Pattern, Type<Real>)>,
}

#[derive(Default)]
pub struct Interface {
    /// The types of the functions.
    pub variables: HashMap<Symbol, LetDef>,

    /// The types of the functions.
    pub constructors: HashMap<Symbol, (Type<Real>, usize)>,

    /// The types of the types.
    pub types: HashMap<Symbol, TypeData>,

    /// The fields of the records.
    pub fields: HashMap<Symbol, Type<Real>>,

    /// The effects of some symbols.
    pub effects: HashMap<Symbol, (Type<Virtual>, usize)>,
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

    pub fn constructor(&mut self, qualified: &Qualified) -> (Type<Real>, usize) {
        let module = self.get(&qualified.path);
        module.constructors.get(&qualified.name).unwrap().clone()
    }

    pub fn effect(&mut self, qualified: &Qualified) -> (Type<Virtual>, usize) {
        let module = self.get(&qualified.path);
        module.effects.get(&qualified.name).unwrap().clone()
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
