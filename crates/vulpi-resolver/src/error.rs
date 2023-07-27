use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::IntoDiagnostic;

pub enum ResolverErrorKind {
    Redeclarated(Symbol),
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
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Span {
        self.span.clone()
    }
}
