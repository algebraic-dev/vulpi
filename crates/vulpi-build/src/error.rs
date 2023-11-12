use std::path::PathBuf;

use vulpi_location::Span;
use vulpi_report::IntoDiagnostic;

pub enum BuildErrorKind {
    NotFound
}

pub struct BuildError {
    pub span: Span,
    pub kind: BuildErrorKind,
}

impl IntoDiagnostic for BuildError {
    fn message(&self) -> vulpi_report::Text {
        match &self.kind {
            BuildErrorKind::NotFound => format!("module not found").into(),
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Span {
        self.span.clone()
    }
}