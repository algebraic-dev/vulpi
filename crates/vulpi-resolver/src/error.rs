//! Data structures for error reporting. The main type is [ResolverError], which is used to report
//! errors during the resolution phase.

use std::ops::Range;

use vulpi_location::Byte;
use vulpi_report::IntoDiagnostic;
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;

pub enum ResolverErrorKind {
    AlreadyCaptured(Symbol),
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
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> vulpi_location::Location {
        vulpi_location::Location::new(self.file, self.range.clone())
    }
}
