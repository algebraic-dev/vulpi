use std::ops::Range;

use vulpi_location::{Byte, Location};
use vulpi_report::{IntoDiagnostic, Marker};
use vulpi_storage::id::{File, Id};
use vulpi_storage::interner::Symbol;

pub enum ParserError {
    UnexpectedToken(Symbol, Range<Byte>, Id<File>),
}

impl IntoDiagnostic for ParserError {
    fn message(&self) -> vulpi_report::Text {
        "unexpected token".into()
    }

    fn markers(&self) -> Vec<vulpi_report::Marker> {
        match self {
            ParserError::UnexpectedToken(_, range, file) => {
                vec![Marker {
                    position: self.location(),
                    subtitle: None,
                }]
            }
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> vulpi_location::Location {
        match self {
            ParserError::UnexpectedToken(_, range, file) => Location {
                file: *file,
                range: range.clone(),
            },
        }
    }
}
