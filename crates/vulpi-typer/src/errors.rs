use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::{IntoDiagnostic, Text};
use vulpi_syntax::r#abstract::Qualified;

use crate::{
    coverage::{Pat, Row},
    r#type::{r#virtual::Env, real::Real, Type},
};

pub enum TypeErrorKind {
    EmptyCase,
    UnboundTypeVariable(Symbol),
    TypeMismatch(Env, Type<Real>, Type<Real>),
    KindMismatch(Env, Type<Real>, Type<Real>),
    InfiniteType,
    CannotFind(Symbol),
    AtLeastOneArgument,
    EscapingScope,
    NotAFunctionKind,
    WrongArity(usize, usize),
    NotAFunction(Env, Type<Real>),
    NotImplemented,
    MissingLabel(Qualified),
    InvalidLabels(Vec<Qualified>),
    PatternsNotAllowedHere,

    DuplicatedField,
    NotFoundField,
    NotARecord,
    MissingField(Symbol),
    NonExhaustive(Row<Pat>),
}

pub struct TypeError {
    pub span: Span,
    pub kind: TypeErrorKind,
}

impl IntoDiagnostic for TypeError {
    fn message(&self) -> Text {
        match &self.kind {
            TypeErrorKind::TypeMismatch(env, left, right) => Text::from(format!(
                "type mismatch: {} != {}",
                left.show(env),
                right.show(env)
            )),
            TypeErrorKind::EmptyCase => Text::from("empty case".to_string()),
            TypeErrorKind::KindMismatch(env, left, right) => Text::from(format!(
                "kind mismatch: {} != {}",
                left.show(env),
                right.show(env),
            )),
            TypeErrorKind::InfiniteType => Text::from("infinite type".to_string()),
            TypeErrorKind::EscapingScope => Text::from("escaping scope".to_string()),
            TypeErrorKind::NotAFunctionKind => Text::from("not a function kind".to_string()),
            TypeErrorKind::UnboundTypeVariable(name) => {
                Text::from(format!("unbound type variable: {}", name.get()))
            }
            TypeErrorKind::WrongArity(expected, found) => Text::from(format!(
                "wrong arity: expected {} arguments, found {}",
                expected, found
            )),
            TypeErrorKind::NotAFunction(env, ty) => {
                Text::from(format!("not a function: {}", ty.show(env)))
            }
            TypeErrorKind::CannotFind(name) => Text::from(format!("cannot find: {}", name.get())),
            TypeErrorKind::NotImplemented => Text::from("not implemented".to_string()),
            TypeErrorKind::DuplicatedField => Text::from("duplicated field".to_string()),
            TypeErrorKind::NotFoundField => Text::from("not found field".to_string()),
            TypeErrorKind::NotARecord => Text::from("not a record".to_string()),
            TypeErrorKind::MissingField(name) => {
                Text::from(format!("missing field: {}", name.get()))
            }
            TypeErrorKind::MissingLabel(name) => {
                Text::from(format!("missing label: {}", name.name.get()))
            }
            TypeErrorKind::InvalidLabels(labels) => Text::from(format!(
                "invalid labels: {}",
                labels
                    .iter()
                    .map(|label| label.name.get())
                    .collect::<Vec<_>>()
                    .join(", ")
            )),

            TypeErrorKind::PatternsNotAllowedHere => {
                Text::from("patterns are not allowed here".to_string())
            }
            TypeErrorKind::AtLeastOneArgument => {
                Text::from("at least one argument is required".to_string())
            }

            TypeErrorKind::NonExhaustive(row) => {
                Text::from(format!("non-exhaustive patterns: {}", row))
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
