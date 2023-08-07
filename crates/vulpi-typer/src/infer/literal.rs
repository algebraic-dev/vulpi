use vulpi_syntax::{r#abstract::Literal, r#abstract::LiteralKind};

use crate::{env::Env, types::Type, Infer};

impl Infer for Literal {
    type Return = Type;

    type Context<'a> = &'a Env;

    fn infer(&self, context: Self::Context<'_>) -> Self::Return {
        match self.data {
            LiteralKind::String(_) => {
                if let Some(name) = context.import("String") {
                    Type::variable(name)
                } else {
                    Type::error()
                }
            }
            LiteralKind::Integer(_) => {
                if let Some(name) = context.import("Int") {
                    Type::variable(name)
                } else {
                    Type::error()
                }
            }
            LiteralKind::Float(_) => {
                if let Some(name) = context.import("Float") {
                    Type::variable(name)
                } else {
                    Type::error()
                }
            }
            LiteralKind::Char(_) => {
                if let Some(name) = context.import("Char") {
                    Type::variable(name)
                } else {
                    Type::error()
                }
            }
            LiteralKind::Unit => Type::tuple(vec![]),
        }
    }
}
