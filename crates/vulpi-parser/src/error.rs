use vulpi_location::Span;
use vulpi_report::IntoDiagnostic;
use vulpi_syntax::tokens::Token;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Box<Token>, Span),
}

impl IntoDiagnostic for ParserError {
    fn message(&self) -> vulpi_report::Text {
        match self {
            ParserError::UnexpectedToken(token, _) => {
                format!("unexpected token '{:?}'", token.kind).into()
            }
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Span {
        match self {
            ParserError::UnexpectedToken(_, span) => span.clone(),
        }
    }
}
