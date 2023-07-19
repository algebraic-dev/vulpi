//! Data structures for error reporting. The main type is [ResolverError], which is used to report
//! errors during the resolution phase.

use std::ops::Range;

use vulpi_location::Byte;
use vulpi_report::IntoDiagnostic;
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;

pub enum ResolverErrorKind {
    AlreadyCaptured(Symbol),
    CannotFindVariable(Symbol),
    CannotFindTypeVariable(Symbol),
    CannotFindModule(Vec<Symbol>),
    VariableAlreadyCaptured(Symbol),
    VariableNotBoundOnBothSides(Symbol),
    CannotFindType(Symbol),
    AmbiguousImport(Symbol),
}

pub struct ResolverError {
    pub message: ResolverErrorKind,
    pub range: Range<Byte>,
    pub file: Id<id::File>,
}

impl IntoDiagnostic for ResolverError {
    fn message(&self) -> vulpi_report::Text {
        match &self.message {
            ResolverErrorKind::AlreadyCaptured(symbol) => {
                format!("Symbol {} is already captured", symbol.get()).into()
            }
            ResolverErrorKind::CannotFindVariable(symbol) => {
                format!("cannot find variable {}", symbol.get()).into()
            }
            ResolverErrorKind::CannotFindTypeVariable(symbol) => {
                format!("cannot find type variable {}", symbol.get()).into()
            }
            ResolverErrorKind::CannotFindModule(symbols) => format!(
                "cannot find module '{}'",
                symbols
                    .iter()
                    .map(|x| x.get())
                    .collect::<Vec<_>>()
                    .join(".")
            )
            .into(),
            ResolverErrorKind::VariableAlreadyCaptured(symbol) => {
                format!("variable {} is already captured", symbol.get()).into()
            }
            ResolverErrorKind::VariableNotBoundOnBothSides(symbol) => {
                format!("variable {} is not bound on both sides", symbol.get()).into()
            }
            ResolverErrorKind::CannotFindType(symbol) => {
                format!("cannot find type {}", symbol.get()).into()
            }
            ResolverErrorKind::AmbiguousImport(symbol) => {
                format!("ambiguous import of {}", symbol.get()).into()
            }
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> vulpi_location::Location {
        vulpi_location::Location::new(self.file, self.range.clone())
    }
}
