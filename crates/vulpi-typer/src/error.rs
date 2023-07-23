//! The error module that is responsible for declaring types for type errors.

use vulpi_location::Location;
use vulpi_report::IntoDiagnostic;
use vulpi_storage::interner::Symbol;

use crate::types::{Kind, Type};

pub enum TypeErrorKind {
    Mismatch(Type, Type),
    MismatchKind(Kind, Kind),
    CannotFindTypeVariable(Symbol),
    CannotInferForall,
    CannotApplyType,
    UnboundVariable(Symbol),
    WrongArity(usize, usize),
    CannotAccessType,
    MismatchArityInPattern(usize, usize),
    ExtraPattern,
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
            TypeErrorKind::MismatchKind(ref left, ref right) => {
                format!("mismatched kinds: expected `{}`, found `{}`", left, right).into()
            }
            TypeErrorKind::UnboundVariable(symbol) => {
                format!("unbound variable {}", symbol.get()).into()
            }
            TypeErrorKind::WrongArity(expected, found) => format!(
                "wrong number of arguments, expected {}, found {}",
                expected, found
            )
            .into(),
            TypeErrorKind::CannotAccessType => "cannot access type".to_string().into(),
            TypeErrorKind::MismatchArityInPattern(expected, found) => format!(
                "wrong number of arguments in pattern, expected {}, found {}",
                expected, found
            )
            .into(),
            TypeErrorKind::ExtraPattern => "extra pattern".to_string().into(),
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Location {
        self.location.clone()
    }
}
