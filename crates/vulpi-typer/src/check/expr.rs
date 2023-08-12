//! Checking of expressions

use vulpi_syntax::r#abstract::Expr;

use crate::{
    r#type::{Effect, TypeKind},
    Context, Env, Type, Virtual,
};

use super::Check;
use crate::infer::Infer;

impl Check for Expr {
    type Return = ();

    type Context<'a> = (&'a mut Context, &'a Effect<Virtual>, Env);

    fn check(
        &self,
        ty: crate::Type<crate::Virtual>,
        (ctx, ambient, env): Self::Context<'_>,
    ) -> Self::Return {
        env.on(self.span.clone());
        match (&self.data, ty.deref().as_ref()) {
            (_, TypeKind::Forall(l)) => {
                let lvl_ty = Type::new(TypeKind::Bound(env.level));
                self.check(
                    l.body.apply_local(Some(l.name.clone()), lvl_ty.clone()),
                    (ctx, ambient, env.add(Some(l.name.clone()), lvl_ty)),
                )
            }
            _ => {
                let expr_ty = self.infer((ctx, ambient, env.clone()));
                ctx.subsumes(env, expr_ty, ty);
            }
        }
    }
}
