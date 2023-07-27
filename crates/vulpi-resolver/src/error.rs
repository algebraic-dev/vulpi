use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::IntoDiagnostic;

pub enum ResolverErrorKind {
    Redeclarated(Symbol),
    NotFound(Vec<Symbol>),
}

pub struct ResolverError {
    pub span: Span,
    pub kind: ResolverErrorKind,
}

impl IntoDiagnostic for ResolverError {
    fn message(&self) -> vulpi_report::Text {
        match &self.kind {
            ResolverErrorKind::Redeclarated(name) => {
                format!("Redeclarated name: {}", name.get()).into()
            }
            ResolverErrorKind::NotFound(name) => format!(
                "name not found: {}",
                name.iter().map(|s| s.get()).collect::<Vec<_>>().join(".")
            )
            .into(),
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Span {
        self.span.clone()
    }
}
