//! The environment. This is the module responsible for the creating a structure called
//! [Env] that is responsible for storing the types of the variables and types of types.

use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::{Diagnostic, Report};

use crate::{
    error::TypeError,
    kind::Kind,
    module::Modules,
    types::{Hole, Type, TypeKind},
};

/// A collection of types of variables and types of types.
#[derive(Clone)]
pub struct Env {
    /// The reporter that is responsible for reporting errors.
    pub reporter: Report,

    /// The level of the environment. This is used for the type checking of the higher rank types.
    pub level: usize,

    /// The types of the variables.
    pub variables: im_rc::HashMap<Symbol, Type>,

    /// The types of the types.
    pub types: im_rc::HashMap<Symbol, crate::kind::Kind>,

    /// Variable names
    pub names: im_rc::Vector<(Symbol, Kind)>,

    /// Counter for name generation
    pub counter: Rc<RefCell<usize>>,

    /// The location of the environment.
    pub location: RefCell<Span>,

    /// The modules.
    pub modules: Rc<RefCell<Modules>>,

    /// The current id of the module.
    pub current_id: Cell<usize>,
}

impl Env {
    pub fn new(reporter: Report, modules: usize) -> Self {
        Self {
            reporter,
            level: 0,
            variables: im_rc::HashMap::new(),
            types: im_rc::HashMap::new(),
            names: im_rc::Vector::new(),
            counter: Rc::new(RefCell::new(0)),
            location: RefCell::new(Span::default()),
            current_id: Cell::new(0),
            modules: Rc::new(RefCell::new(Modules::new(modules))),
        }
    }

    pub fn set_module(&self, id: usize) {
        self.current_id.replace(id);
    }

    pub fn current_id(&self) -> usize {
        self.current_id.get()
    }

    pub fn new_hole(&self) -> Type {
        Type::new(TypeKind::Hole(Hole::new(self.level)))
    }

    pub fn set_location(&self, location: Span) {
        *self.location.borrow_mut() = location;
    }

    pub fn new_name(&self) -> Symbol {
        let mut counter = self.counter.borrow_mut();
        let name = Symbol::intern(&format!("t{}", *counter));
        *counter += 1;
        name
    }

    pub fn report(&self, error: crate::error::TypeErrorKind) {
        self.reporter.report(Diagnostic::new(TypeError {
            span: self.location.borrow().clone(),
            kind: error,
        }));
    }

    /// Adds a new type in the environment. It's useful for the type checking of the higher rank types.
    pub fn add_new_ty(&self, kind: Kind) -> Env {
        let mut new_env = self.clone();
        new_env.level += 1;
        let name = new_env.new_name();

        new_env.names.push_back((name, kind));

        new_env
    }

    pub fn add_ty(&self, name: Symbol, kind: Kind) -> Env {
        let mut new_env = self.clone();
        new_env.types.insert(name, kind);
        new_env
    }

    pub fn get_ty(&self, name: &Symbol) -> Option<Kind> {
        self.types.get(name).cloned()
    }
}
