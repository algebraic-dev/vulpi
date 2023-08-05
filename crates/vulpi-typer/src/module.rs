//! Module for declaration of top level items inside the type checker. The main structure of this
//! module is the [Module] structure that is responsible for storing the types of the top level
//! items.

use std::collections::HashMap;

use vulpi_intern::Symbol;

#[derive(Clone)]
pub struct TypeData {
    pub kind: crate::kind::Kind,
    pub module: Symbol,
}

#[derive(Default, Clone)]
pub struct Module {
    /// The types of the functions.
    pub variables: im_rc::HashMap<Symbol, crate::types::Type>,

    /// The types of the functions.
    pub constructors: im_rc::HashMap<Symbol, (crate::types::Type, usize)>,

    /// The types of the types.
    pub types: im_rc::HashMap<Symbol, TypeData>,

    /// The fields of the records.
    pub fields: im_rc::HashMap<Symbol, crate::types::Type>,

    /// The effects of some symbols.
    pub effects: im_rc::HashMap<Symbol, crate::types::Type>,
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

    pub fn get(&mut self, id: Symbol) -> &mut Module {
        self.modules.entry(id).or_default()
    }
}
