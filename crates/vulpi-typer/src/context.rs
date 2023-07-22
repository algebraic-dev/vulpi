//! This module defines a [Env] that is responsible for storing types, variables and other things
//! It is also responsible to report errors and store the `level`. Each context contains a `level`
//! to identify where it is in the type checking process.

use std::ops::Range;
use std::{cell::RefCell, rc::Rc};

use vulpi_location::{Byte, Location};
use vulpi_report::{Diagnostic, Report};
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;

use crate::error::{TypeError, TypeErrorKind};
use crate::types::{Hole, HoleInner, Kind, Level, Mono, Scheme, Type};
use crate::Modules;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Qualified {
    pub path: Symbol,
    pub name: Symbol,
}

/// The env is responsible for storing types and variables.
#[derive(Clone)]
pub struct Env {
    /// Variables that are in scope and their types.
    pub variables: im_rc::HashMap<Symbol, Scheme>,

    /// Injected variables that corresponds to things declared in
    /// other modules or in this module.
    pub modules: Rc<RefCell<Modules>>,

    /// Type variables that are in scope.
    pub type_variables: im_rc::HashMap<Symbol, (Kind, usize)>,

    /// The reporter that is used to report errors.
    pub reporter: Report,

    /// The level is the place that we are inside the typ checking process. The level increases
    /// each time we enter inside a generalization scope.
    pub level: RefCell<Level>,

    /// Location of the expression that we are type checking.
    pub location: RefCell<vulpi_location::Location>,

    /// Counter for name generation
    pub counter: Rc<RefCell<usize>>,

    /// File identifier
    pub file: Id<id::File>,
}

impl Env {
    pub fn new(reporter: Report, file: Id<id::File>, modules: Rc<RefCell<Modules>>) -> Self {
        Self {
            modules,
            variables: Default::default(),
            type_variables: Default::default(),
            reporter,
            level: RefCell::new(Level(0)),
            location: RefCell::new(Location {
                file,
                range: Byte(0)..Byte(0),
            }),
            counter: Rc::new(RefCell::new(0)),
            file,
        }
    }

    pub fn get_global_type(&self, id: Id<id::Namespace>, name: &Symbol) -> Option<Kind> {
        self.modules
            .borrow()
            .modules
            .get(&id)?
            .types
            .get(name)
            .cloned()
    }

    pub fn set_global_type(&mut self, id: Id<id::Namespace>, name: Symbol, kind: Kind) {
        self.modules
            .borrow_mut()
            .modules
            .entry(id)
            .or_default()
            .types
            .insert(name, kind);
    }

    pub fn set_location(&self, range: Range<Byte>) {
        *self.location.borrow_mut() = Location {
            file: self.file,
            range,
        };
    }

    /// Create a environment based on the last one but with the level increased
    pub fn increase_level(&self) {
        let inc = self.level.borrow().inc();
        *self.level.borrow_mut() = inc;
    }

    /// Create a environment based on the last one but with the level decreased
    pub fn decrease_level(&self) {
        let inc = self.level.borrow().inc();
        *self.level.borrow_mut() = inc;
    }

    /// Adds a new variable to the environment.
    pub fn add_variable(&mut self, name: Symbol, scheme: Scheme) {
        self.variables.insert(name, scheme);
    }

    /// Adds a new variable to the environment.
    pub fn add(&mut self, name: Symbol, mono: Type) {
        self.add_variable(name, Scheme::new(vec![], mono));
    }

    /// Adds a new type variable to the environment.
    pub fn add_type_variable(&mut self, name: Symbol, kind: Kind, place: usize) {
        self.type_variables.insert(name, (kind, place));
    }

    /// Gets a variable from the environment.
    pub fn get_variable(&self, name: Symbol) -> Option<&Scheme> {
        self.variables.get(&name)
    }

    /// Reports a type error.
    pub fn report(&self, kind: TypeErrorKind) {
        self.reporter.report(Diagnostic::new(TypeError {
            location: self.location.borrow().clone(),
            kind,
        }));
    }

    /// Generates a new name for a type variable.
    pub fn new_name(&mut self) -> Symbol {
        let mut counter = self.counter.borrow_mut();
        let name = Symbol::intern(&format!("t{}", *counter));
        *counter += 1;
        name
    }

    /// Creates a new hole.
    pub fn new_hole(&mut self) -> Type {
        Type::new(Mono::Hole(Hole::new(self.new_name(), *self.level.borrow())))
    }

    /// Instantiates a scheme into a mono type.
    pub fn instantiate(&mut self, scheme: Scheme) -> Type {
        let new_vars = scheme
            .variables
            .iter()
            .map(|_| self.new_hole())
            .collect::<Vec<_>>();

        scheme.monotype.instantiate_with(&new_vars)
    }

    /// Generalizes a type into a scheme.
    pub fn generalize(&mut self, typ: Type) -> Scheme {
        pub fn gen(ambient: Level, typ: Type, counter: &mut usize) {
            match &&*typ {
                Mono::Hole(hole) => match hole.get() {
                    HoleInner::Unbound(n, level) if level.0 > ambient.0 => {
                        let lvl = *counter;
                        *counter += 1;
                        hole.fill(Type::new(Mono::Generalized(lvl, n)));
                    }
                    HoleInner::Unbound(_, _) => (),
                    HoleInner::Link(f) => gen(ambient, f, counter),
                },
                Mono::Function(l, r) => {
                    gen(ambient, l.clone(), counter);
                    gen(ambient, r.clone(), counter);
                }
                _ => (),
            }
        }
        let mut counter = 0;
        let names = (0..counter).map(|_| self.new_name()).collect::<Vec<_>>();
        gen(*self.level.borrow(), typ.clone(), &mut counter);
        Scheme::new(names, typ)
    }
}
