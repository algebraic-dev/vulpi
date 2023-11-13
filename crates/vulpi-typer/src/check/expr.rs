//! Checking of expressions

use vulpi_syntax::{elaborated, r#abstract::Expr, r#abstract::ExprKind, r#abstract::Sttm};

use crate::{
    r#type::TypeKind,
    Context, Env, Real, Type, Virtual,
};

use super::Check;
use crate::infer::Infer;

impl Check for Expr {
    type Return = elaborated::Expr<Type<Real>>;

    type Context<'a> = (&'a mut Context, Env);

    fn check(
        &self,
        ty: crate::Type<crate::Virtual>,
        (ctx, mut env): Self::Context<'_>,
    ) -> Self::Return {
        env.on(self.span.clone());
        match (&self.data, ty.deref().as_ref()) {
            (ExprKind::Do(block), _) => {
                let mut stmts = Vec::new();

                if !block.sttms.is_empty() {
                    for (i, stmt) in block.sttms.iter().enumerate() {
                        let is_last = i == block.sttms.len() - 1;
                        let (elab, new_env) = if is_last {
                            stmt.check(ty.clone(), (ctx, env.clone()))
                        } else {
                            let (_, new_env, elab) = stmt.infer((ctx, &mut env.clone()));
                            (elab, new_env)
                        };

                        env = new_env;

                        stmts.push(elab)
                    }
                }

                Box::new(elaborated::ExprKind::Do(stmts))
            }
            (_, TypeKind::Forall(l)) => {
                let lvl_ty = Type::new(TypeKind::Bound(env.level));
                self.check(
                    l.body.apply_local(Some(l.name.clone()), lvl_ty.clone()),
                    (ctx, env.add(Some(l.name.clone()), lvl_ty)),
                )
            }
            _ => {
                let (expr_ty, elab_expr) = self.infer((ctx, env.clone()));
                ctx.subsumes(env, expr_ty, ty);
                elab_expr
            }
        }
    }
}

impl Check for Sttm {
    type Return = (elaborated::Statement<Type<Real>>, Env);

    type Context<'a> = (&'a mut Context, Env);

    fn check(&self, ty: Type<Virtual>, (ctx, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        let (typ, env, elab) = self.infer((ctx, &mut env.clone()));

        ctx.subsumes(env.clone(), typ, ty);
        (elab, env)
    }
}