//! Checking of expressions

use vulpi_location::Spanned;
use vulpi_syntax::{elaborated, r#abstract::Expr, r#abstract::ExprKind, r#abstract::Sttm};

use crate::{context::Context, real::Real, Env, Type, TypeKind, Virtual};

use super::Check;
use crate::infer::Infer;

impl Check for Expr {
    type Return = elaborated::Expr<Type<Real>>;

    type Context<'a> = (&'a mut Context, Env);

    fn check(
        &self,
        typ: crate::Type<crate::Virtual>,
        (ctx, mut env): Self::Context<'_>,
    ) -> Self::Return {
        env.set_current_span(self.span.clone());

        let elem = match (&self.data, typ.deref().as_ref()) {
            (ExprKind::Do(block), _) => {
                let mut stmts = Vec::new();

                if !block.sttms.is_empty() {
                    for (i, stmt) in block.sttms.iter().enumerate() {
                        let is_last = i == block.sttms.len() - 1;
                        let (elab, new_env) = if is_last {
                            stmt.check(typ.clone(), (ctx, env.clone()))
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
                .data
            }
            _ => {
                let (expr_ty, elab_expr) = self.infer((ctx, env.clone()));
                ctx.subsumes(env, expr_ty, typ);
                elab_expr.data
            }
        };

        Spanned::new(elem, self.span.clone())
    }
}

impl Check for Sttm {
    type Return = (elaborated::Statement<Type<Real>>, Env);

    type Context<'a> = (&'a mut Context, Env);

    fn check(&self, ann_ty: Type<Virtual>, (ctx, env): Self::Context<'_>) -> Self::Return {
        env.set_current_span(self.span.clone());

        let (typ, env, elab) = self.infer((ctx, &mut env.clone()));

        ctx.subsumes(env.clone(), typ, ann_ty);
        (elab, env)
    }
}
