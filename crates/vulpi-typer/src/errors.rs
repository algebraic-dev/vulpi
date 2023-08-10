use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::{IntoDiagnostic, Text};

use crate::r#type::{r#virtual::Env, r#virtual::Virtual, real::Real, Type};

pub enum TypeErrorKind {
    UnboundTypeVariable(Symbol),
    TypeMismatch(Env, Type<Real>, Type<Real>),
    KindMismatch(Env, Type<Real>, Type<Real>),
    InfiniteType,
    CannotFind(Symbol),
    EscapingScope,
    NotAFunctionKind,
    WrongArity(usize, usize),
    NotAFunction(Env, Type<Real>),
    NotImplemented,
    NotEffect,

    DuplicatedField,
    NotFoundField,
    NotARecord,
    MissingField(Symbol),
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
            TypeErrorKind::NotEffect => Text::from("not effect".to_string()),
        }
    }

    fn severity(&self) -> vulpi_report::Severity {
        vulpi_report::Severity::Error
    }

    fn location(&self) -> Span {
        self.span.clone()
    }
}
