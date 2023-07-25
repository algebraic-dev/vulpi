//! Error types for the lexing process. These are converted into [vulpi_report::Diagnostic].

use vulpi_location::Span;
use vulpi_report::IntoDiagnostic;

/// The kind of lexing error.
pub enum ErrorKind {
    UnfinishedString,
}

/// A lexing error.
pub struct Error {
    pub location: Span,
    pub message: ErrorKind,
}

impl IntoDiagnostic for Error {
    fn message(&self) -> vulpi_report::Text {
        match self.message {
            ErrorKind::UnfinishedString => vulpi_report::Text::from("unfinished string literal"),
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Span {
        self.location.clone()
    }
}
