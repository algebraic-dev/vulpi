//! Module for declaration of top level items inside the type checker. The main structure of this
//! module is the [Module] structure that is responsible for storing the types of the top level
//! items.

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract::Qualified;

use crate::r#type::{r#virtual::Virtual, Type};

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

#[derive(Clone)]
pub struct LetDef {
    pub typ: Type<Virtual>,
    pub binders: HashMap<Symbol, Type<Virtual>>,
    pub unbound: Vec<(Symbol, Type<Virtual>)>,
    pub unbound_effects: Vec<(Symbol, Type<Virtual>)>,
}

#[derive(Default, Clone)]
pub struct Module {
    /// The types of the functions.
    pub variables: im_rc::HashMap<Symbol, LetDef>,

    /// The types of the functions.
    pub constructors: im_rc::HashMap<Symbol, (Type<Virtual>, usize)>,

    /// The types of the types.
    pub types: im_rc::HashMap<Symbol, TypeData>,

    /// The fields of the records.
    pub fields: im_rc::HashMap<Symbol, Type<Virtual>>,

    /// The effects of some symbols.
    pub effects: im_rc::HashMap<Symbol, (Type<Virtual>, usize)>,
}

#[derive(Default)]
pub struct Modules {
    /// The modules.
    pub modules: HashMap<Symbol, Module>,
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

    pub fn constructor(&mut self, qualified: &Qualified) -> (Type<Virtual>, usize) {
        let module = self.get(&qualified.path);
        module.constructors.get(&qualified.name).unwrap().clone()
    }

    pub fn effect(&mut self, qualified: &Qualified) -> (Type<Virtual>, usize) {
        let module = self.get(&qualified.path);
        module.effects.get(&qualified.name).unwrap().clone()
    }

    pub fn get(&mut self, id: &Symbol) -> &mut Module {
        self.modules.entry(id.clone()).or_default()
    }
}
