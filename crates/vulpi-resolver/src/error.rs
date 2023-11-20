use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::IntoDiagnostic;
use vulpi_syntax::r#abstract::Qualified;

pub enum ResolverErrorKind {
    NotFound(Symbol),
    InvalidPath(Vec<Symbol>),
    DuplicatePattern(Symbol),
    PrivateDefinition,
    CycleBetweenConstants(Vec<Qualified>),
    NotImplemented(Symbol, Symbol),
}

pub struct ResolverError {
    pub span: Span,
    pub kind: ResolverErrorKind,
}

impl IntoDiagnostic for ResolverError {
    fn message(&self) -> vulpi_report::Text {
        match &self.kind {
            ResolverErrorKind::NotImplemented(name, feature) => format!(
                "the method '{}' is not present in the trait '{}'",
                feature.get(),
                name.get()
            )
            .into(),
            ResolverErrorKind::NotFound(name) => format!("cannot find '{}'", name.get()).into(),
            ResolverErrorKind::InvalidPath(name) => format!(
                "the path '{}' cannot be found",
                name.iter().map(|s| s.get()).collect::<Vec<_>>().join(".")
            )
            .into(),
            ResolverErrorKind::DuplicatePattern(name) => {
                format!("duplicate pattern: {}", name.get()).into()
            }
            ResolverErrorKind::PrivateDefinition => "private definition".into(),
            ResolverErrorKind::CycleBetweenConstants(cycle) => {
                let mut cycle = cycle.iter().map(|q| q.to_string()).collect::<Vec<_>>();
                cycle.sort_by_key(|k| k.to_string());

                format!("cycle between '{}'", cycle.join(" -> ")).into()
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
