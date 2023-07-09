use vulpi_location::Location;
use vulpi_report::IntoDiagnostic;
use vulpi_storage::interner::Symbol;

use crate::{Path, Qualified};

pub enum ResolverErrorKind {
    CantFindModule(Path),
    DuplicatedTypeVariable(Symbol),
    TypeVariableNotInScope(Symbol),
    UnboundType(Qualified),
    DuplicatePatternVariable(Symbol),
    Ambiguity(Path),
    CantFindType(Qualified),
    CantFindValue(Qualified),
}

pub struct ResolverError {
    pub location: Location,
    pub kind: ResolverErrorKind,
}

impl IntoDiagnostic for ResolverError {
    fn message(&self) -> vulpi_report::Text {
        match &self.kind {
            ResolverErrorKind::CantFindModule(path) => {
                format!("can't find module `{}`", path).into()
            }
            ResolverErrorKind::DuplicatedTypeVariable(name) => {
                format!("duplicated type variable `{}`", name.get()).into()
            }
            ResolverErrorKind::TypeVariableNotInScope(name) => {
                format!("type variable `{}` is not in scope", name.get()).into()
            }
            ResolverErrorKind::DuplicatePatternVariable(name) => {
                format!("duplicated pattern variable `{}`", name.get()).into()
            }
            ResolverErrorKind::CantFindType(name) => format!("can't find type `{}`", name).into(),
            ResolverErrorKind::CantFindValue(name) => format!("can't find `{}`", name).into(),
            ResolverErrorKind::UnboundType(name) => format!("unbound type `{}`", name).into(),
            ResolverErrorKind::Ambiguity(name) => format!("ambiguous name `{}`", name).into(),
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

pub type Result<T> = std::result::Result<T, ResolverError>;
