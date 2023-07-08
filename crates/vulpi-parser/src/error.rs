use std::ops::Range;

use vulpi_location::{Byte, Location};
use vulpi_report::{IntoDiagnostic, Marker};
use vulpi_storage::id::{File, Id};
use vulpi_syntax::token::Token;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token, Range<Byte>, Id<File>),
}

impl IntoDiagnostic for ParserError {
    fn message(&self) -> vulpi_report::Text {
        match self {
            ParserError::UnexpectedToken(token, _, _) => {
                format!("unexpected token: {:?}", token.data).into()
            }
        }
    }

    fn markers(&self) -> Vec<vulpi_report::Marker> {
        match self {
            ParserError::UnexpectedToken(_, _, _) => {
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
