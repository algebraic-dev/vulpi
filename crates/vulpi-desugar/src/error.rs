use vulpi_location::Location;
use vulpi_report::IntoDiagnostic;
use vulpi_storage::interner::Symbol;

pub enum ErrorKind {
    OutOfOrderDefinition(Symbol),
    Redeclaration(Symbol),
}

pub struct Error {
    pub kind: ErrorKind,
    pub location: Location,
}

impl IntoDiagnostic for Error {
    fn message(&self) -> vulpi_report::Text {
        match &self.kind {
            ErrorKind::OutOfOrderDefinition(name) => format!(
                "the definition `{}` should be with all of the other definitions.",
                name.get()
            )
            .into(),
            ErrorKind::Redeclaration(name) => {
                format!("the name `{}` has already been declared.", name.get()).into()
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
