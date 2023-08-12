//! Inference of literals

use vulpi_syntax::{r#abstract::Literal, r#abstract::LiteralKind, r#abstract::Qualified};

use super::Infer;
use crate::{r#type::r#virtual::Virtual, Context, Env, Type};
use vulpi_intern::Symbol;

impl Infer for Literal {
    type Return = Type<Virtual>;

    type Context<'a> = (&'a mut Context, Env);

    fn infer(&self, (ctx, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        match &self.data {
            LiteralKind::String(_) => find_prelude_type("String", ctx, env),
            LiteralKind::Integer(_) => find_prelude_type("Int", ctx, env),
            LiteralKind::Float(_) => find_prelude_type("Float", ctx, env),
            LiteralKind::Char(_) => find_prelude_type("Char", ctx, env),
            LiteralKind::Unit => Type::tuple(vec![]),
        }
    }
}

pub fn find_prelude_type(name: &str, ctx: &mut Context, env: Env) -> Type<Virtual> {
    let path = Symbol::intern("Prelude");
    let name = Symbol::intern(name);
    if ctx.modules.get(&path).types.get(&name).is_some() {
        Type::variable(Qualified { path, name })
    } else {
        ctx.report(&env, crate::errors::TypeErrorKind::CannotFind(name));
        Type::error()
    }
}
