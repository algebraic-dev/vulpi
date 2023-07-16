#![allow(incomplete_features)]
#![feature(specialization)]

//! Module to resolve symbols in a module. This is the third stage of the compiler pipeline and it
//! checks for the following:
//!
//! - All variables inside a pattern are linear
//!

pub mod error;

use std::{collections::HashSet, ops::Range};

use error::{ResolverError, ResolverErrorKind};

use vulpi_location::Byte;
use vulpi_report::{Diagnostic, Report};
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;
use vulpi_syntax::r#abstract::Program;
use vulpi_syntax::r#abstract::{PatLower, PatternImpl, Visitor};

/// The resolver context. This is used to keep track of the symbols that are captured by patterns,
/// and to report errors.
pub struct Context {
    pub captured: HashSet<Symbol>,
    pub is_capturing: bool,
    pub reporter: Report,
    pub file: Id<id::File>,
}

impl Context {
    pub fn new(reporter: Report, file: Id<id::File>) -> Self {
        Self {
            file,
            reporter,
            captured: HashSet::new(),
            is_capturing: false,
        }
    }

    pub fn capture(&mut self, symbol: Symbol, range: Range<Byte>) {
        if self.is_captured(symbol.clone()) {
            self.report(ResolverErrorKind::AlreadyCaptured(symbol), range);
        } else {
            self.captured.insert(symbol);
        }
    }

    pub fn is_captured(&self, symbol: Symbol) -> bool {
        self.captured.contains(&symbol)
    }

    pub fn start_capture(&mut self) {
        self.is_capturing = true;
    }

    pub fn end_capture(&mut self) {
        self.is_capturing = false;
    }

    pub fn report(&self, message: ResolverErrorKind, range: Range<Byte>) {
        self.reporter.report(Diagnostic::new(ResolverError {
            message,
            range,
            file: self.file,
        }));
    }
}

impl Visitor for Context {
    fn visit_pattern_impl(&mut self, node: &mut Box<dyn PatternImpl>) {
        let is_first = !self.is_capturing;
        self.is_capturing = true;
        node.accept(self);
        if is_first {
            self.captured = HashSet::new();
            self.is_capturing = false;
        }
    }

    fn visit_pat_lower(&mut self, node: &mut PatLower) {
        if self.is_capturing {
            self.capture(node.0 .0.clone(), node.0 .1.clone());
        }
    }
}

pub fn resolve(mut node: Program, file: Id<id::File>, reporter: Report) -> Program {
    let mut context = Context::new(reporter, file);
    context.visit_program(&mut node);
    node
}
