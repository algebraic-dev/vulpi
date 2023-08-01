use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::{IntoDiagnostic, Text};

use crate::{env::Env, kind::Kind, types::Type};

pub enum TypeErrorKind {
    UnboundTypeVariable(Symbol),
    TypeMismatch(Env, Type, Type),
    KindMismatch(Kind, Kind),
    InfiniteType,
    EscapingScope,
    NotAFunctionKind,
    WrongArity(usize, usize),
    NotAFunction(Env, Type),
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
                left.show(env.clone()),
                right.show(env.clone())
            )),
            TypeErrorKind::KindMismatch(left, right) => {
                Text::from(format!("kind mismatch: {} != {}", left, right,))
            }
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
                Text::from(format!("not a function: {}", ty.show(env.clone())))
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
