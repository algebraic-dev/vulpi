//! Inference of literals

use vulpi_syntax::{elaborated, r#abstract::Literal, r#abstract::LiteralKind};

use super::Infer;
use crate::{r#type::r#virtual::Virtual, Context, Env, Type};

impl Infer for Literal {
    type Return = (Type<Virtual>, elaborated::Literal);

    type Context<'a> = (&'a mut Context, Env);

    fn infer(&self, (ctx, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        match &self.data {
            LiteralKind::String(n) => (
                ctx.find_prelude_type("String", env),
                Box::new(elaborated::LiteralKind::String(n.clone())),
            ),
            LiteralKind::Integer(n) => (
                ctx.find_prelude_type("Int", env),
                Box::new(elaborated::LiteralKind::Integer(n.clone())),
            ),
            LiteralKind::Float(n) => (
                ctx.find_prelude_type("Float", env),
                Box::new(elaborated::LiteralKind::Float(n.clone())),
            ),
            LiteralKind::Char(n) => (
                ctx.find_prelude_type("Char", env),
                Box::new(elaborated::LiteralKind::Char(n.clone())),
            ),
            LiteralKind::Unit => (Type::tuple(vec![]), Box::new(elaborated::LiteralKind::Unit)),
        }
    }
}
