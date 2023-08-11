use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::IntoDiagnostic;

pub enum ResolverErrorKind {
    Redeclarated(Symbol),
    NotFound(Symbol),
    InvalidPath(Vec<Symbol>),
    IsAModule,
    ExpectedConstructor,
    ExpectedRecordType,
    ExpectedEffect,
    ExpectedFunction,
    VariableNotBoundOnBothSides(Symbol),
    DuplicatePattern(Symbol),
    CannotHavePolymorphiEffectInTheMiddle,
    PrivateDefinition,
}

pub struct ResolverError {
    pub span: Span,
    pub kind: ResolverErrorKind,
}

impl IntoDiagnostic for ResolverError {
    fn message(&self) -> vulpi_report::Text {
        match &self.kind {
            ResolverErrorKind::Redeclarated(name) => {
                format!("redeclarated name: {}", name.get()).into()
            }
            ResolverErrorKind::NotFound(name) => format!("name not found: {}", name.get()).into(),
            ResolverErrorKind::InvalidPath(name) => format!(
                "invalid path: {}",
                name.iter().map(|s| s.get()).collect::<Vec<_>>().join(".")
            )
            .into(),
            ResolverErrorKind::IsAModule => "is a module".into(),
            ResolverErrorKind::ExpectedConstructor => "expected constructor".into(),
            ResolverErrorKind::ExpectedEffect => "expected effect".into(),
            ResolverErrorKind::ExpectedFunction => "expected function".into(),
            ResolverErrorKind::ExpectedRecordType => "expected record type".into(),
            ResolverErrorKind::VariableNotBoundOnBothSides(name) => {
                format!("variable not bound on both sides: {}", name.get()).into()
            }
            ResolverErrorKind::DuplicatePattern(name) => {
                format!("duplicate pattern: {}", name.get()).into()
            }
            ResolverErrorKind::PrivateDefinition => "private definition".into(),
            ResolverErrorKind::CannotHavePolymorphiEffectInTheMiddle => {
                "cannot have polymorphic effect in the middle".into()
            }
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Span {
        self.span.clone()
    }
}
