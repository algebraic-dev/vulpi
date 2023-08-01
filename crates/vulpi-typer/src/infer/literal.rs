use vulpi_intern::Symbol;
use vulpi_syntax::{r#abstract::Literal, r#abstract::LiteralKind};

use crate::{env::Env, types::Type, Infer};

impl Infer for Literal {
    type Return = Type;

    type Context<'a> = &'a Env;

    fn infer(&self, context: Self::Context<'_>) -> Self::Return {
        match self.data {
            LiteralKind::String(_) => {
                let name = context
                    .imports
                    .get(&Symbol::intern("String"))
                    .unwrap()
                    .clone();
                Type::variable(name)
            }
            LiteralKind::Integer(_) => {
                let name = context.imports.get(&Symbol::intern("Int")).unwrap().clone();
                Type::variable(name)
            }
            LiteralKind::Float(_) => {
                let name = context
                    .imports
                    .get(&Symbol::intern("Float"))
                    .unwrap()
                    .clone();
                Type::variable(name)
            }
            LiteralKind::Char(_) => {
                let name = context
                    .imports
                    .get(&Symbol::intern("Char"))
                    .unwrap()
                    .clone();
                Type::variable(name)
            }
            LiteralKind::Unit => {
                let name = context
                    .imports
                    .get(&Symbol::intern("Unit"))
                    .unwrap()
                    .clone();
                Type::variable(name)
            }
        }
    }
}
