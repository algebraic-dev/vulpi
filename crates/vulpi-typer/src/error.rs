//! The error module that is responsible for declaring types for type errors.

use vulpi_location::Location;
use vulpi_report::IntoDiagnostic;

use crate::types::Type;

pub enum TypeErrorKind {
    Mismatch(Type, Type),
}

pub struct TypeError {
    pub location: Location,
    pub kind: TypeErrorKind,
}

impl IntoDiagnostic for TypeError {
    fn message(&self) -> vulpi_report::Text {
        match self.kind {
            TypeErrorKind::Mismatch(ref left, ref right) => {
                format!("mismatched types: expected `{}`, found `{}`", left, right).into()
            }
        }
    }

    fn markers(&self) -> Vec<vulpi_report::Marker> {
        vec![]
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Location {
        self.location.clone()
    }
}
