use vulpi_intern::Symbol;
use vulpi_syntax::{r#abstract::Literal, r#abstract::LiteralKind};

use crate::{env::Env, types::Type, Infer};

impl Infer for Literal {
    type Return = Type;

    type Context<'a> = &'a Env;

    fn infer(&self, context: Self::Context<'_>) -> Self::Return {
        match self.data {
            LiteralKind::String(_) => {
                if let Some(name) = context.imports.get(&Symbol::intern("String")) {
                    Type::variable(name.clone())
                } else {
                    Type::error()
                }
            }
            LiteralKind::Integer(_) => {
                if let Some(name) = context.imports.get(&Symbol::intern("Int")) {
                    Type::variable(name.clone())
                } else {
                    Type::error()
                }
            }
            LiteralKind::Float(_) => {
                if let Some(name) = context.imports.get(&Symbol::intern("Float")) {
                    Type::variable(name.clone())
                } else {
                    Type::error()
                }
            }
            LiteralKind::Char(_) => {
                if let Some(name) = context.imports.get(&Symbol::intern("Char")) {
                    Type::variable(name.clone())
                } else {
                    Type::error()
                }
            }
            LiteralKind::Unit => {
                if let Some(name) = context.imports.get(&Symbol::intern("Unit")) {
                    Type::variable(name.clone())
                } else {
                    Type::error()
                }
            }
        }
    }
}
