//! The environment. This is the module responsible for the creating a structure called
//! [Env] that is responsible for storing the types of the variables and types of types.

use std::{cell::RefCell, rc::Rc};

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::{Diagnostic, Report};

use crate::{
    error::TypeError,
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
    pub names: im_rc::Vector<Symbol>,

    /// Counter for name generation
    pub counter: Rc<RefCell<usize>>,

    /// The location of the environment.
    pub location: RefCell<Span>,
}

impl Env {
    pub fn new_hole(&self) -> Type {
        Type::new(TypeKind::Hole(Hole::new(self.level)))
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
}
