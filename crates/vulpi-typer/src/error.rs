//! The error module that is responsible for declaring types for type errors.

use vulpi_location::Location;
use vulpi_report::IntoDiagnostic;
use vulpi_storage::interner::Symbol;

use crate::types::Type;

pub enum TypeErrorKind {
    Mismatch(Type, Type),
    CannotFindTypeVariable(Symbol),
    CannotInferForall,
    CannotApplyType,
}

pub struct TypeError {
    pub location: Location,
    pub kind: TypeErrorKind,
}

impl IntoDiagnostic for TypeError {
    fn message(&self) -> vulpi_report::Text {
        match &self.kind {
            TypeErrorKind::Mismatch(ref left, ref right) => {
                format!("mismatched types: expected `{}`, found `{}`", left, right).into()
            }
            TypeErrorKind::CannotFindTypeVariable(symbol) => {
                format!("cannot find type variable {}", symbol.get()).into()
            }
            TypeErrorKind::CannotInferForall => "cannot infer type for forall".to_string().into(),
            TypeErrorKind::CannotApplyType => "cannot apply type".to_string().into(),
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Location {
        self.location.clone()
    }
}
