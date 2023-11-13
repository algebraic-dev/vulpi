use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::IntoDiagnostic;

pub enum ResolverErrorKind {
    NotFound(Symbol),
    InvalidPath(Vec<Symbol>),
    DuplicatePattern(Symbol),
    PrivateDefinition,
}

pub struct ResolverError {
    pub span: Span,
    pub kind: ResolverErrorKind,
}

impl IntoDiagnostic for ResolverError {
    fn message(&self) -> vulpi_report::Text {
        match &self.kind {
            ResolverErrorKind::NotFound(name) => format!("name not found: {}", name.get()).into(),
            ResolverErrorKind::InvalidPath(name) => format!(
                "the path '{}' cannot be found",
                name.iter().map(|s| s.get()).collect::<Vec<_>>().join(".")
            )
            .into(),
            ResolverErrorKind::DuplicatePattern(name) => {
                format!("duplicate pattern: {}", name.get()).into()
            }
            ResolverErrorKind::PrivateDefinition => "private definition".into(),
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Span {
        self.span.clone()
    }
}