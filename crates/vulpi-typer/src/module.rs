//! Module for declaration of top level items inside the type checker. The main structure of this
//! module is the [Module] structure that is responsible for storing the types of the top level
//! items.

use vulpi_intern::Symbol;

#[derive(Default, Clone)]
pub struct Module {
    /// The types of the functions.
    pub variables: im_rc::HashMap<Symbol, crate::types::Type>,

    /// The types of the functions.
    pub constructors: im_rc::HashMap<Symbol, crate::types::Type>,

    /// The types of the types.
    pub types: im_rc::HashMap<Symbol, crate::kind::Kind>,

    /// The fields of the records.
    pub fields: im_rc::HashMap<Symbol, crate::types::Type>,

    /// The effects of some symbols.
    pub effects: im_rc::HashMap<Symbol, crate::types::Type>,
}

#[derive(Default)]
pub struct Modules {
    /// The modules.
    pub modules: Vec<Module>,
}

impl Modules {
    pub fn new(size: usize) -> Self {
        Self {
            modules: vec![Module::default(); size],
        }
    }
    /// Adds a new module to the modules.
    pub fn add_module(&mut self, module: Module) {
        self.modules.push(module);
    }

    pub fn get(&self, id: usize) -> Option<&Module> {
        self.modules.get(id)
    }
}
