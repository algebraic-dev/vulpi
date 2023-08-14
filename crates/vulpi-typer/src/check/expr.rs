//! Checking of expressions

use vulpi_syntax::{elaborated, r#abstract::Expr, r#abstract::ExprKind, r#abstract::Statement};

use crate::{
    errors::TypeErrorKind,
    infer::pat::EffectPat,
    r#type::{r#virtual, Effect, TypeKind},
    Context, Env, Kind, Real, Type, Virtual,
};

use super::Check;
use crate::infer::Infer;

impl Check for Expr {
    type Return = elaborated::Expr<Type<Real>>;

    type Context<'a> = (&'a mut Context, &'a Effect<Virtual>, Env);

    fn check(
        &self,
        ty: crate::Type<crate::Virtual>,
        (ctx, ambient, mut env): Self::Context<'_>,
    ) -> Self::Return {
        env.on(self.span.clone());
        match (&self.data, ty.deref().as_ref()) {
            (ExprKind::Do(block), _) => {
                let mut stmts = Vec::new();

                if !block.statements.is_empty() {
                    for (i, stmt) in block.statements.iter().enumerate() {
                        let is_last = i == block.statements.len() - 1;
                        let (elab, new_env) = if is_last {
                            stmt.check(ty.clone(), (ctx, ambient, env.clone()))
                        } else {
                            let (_, new_env, elab) = stmt.infer((ctx, ambient, env.clone()));
                            (elab, new_env)
                        };

                        env = new_env;

                        stmts.push(elab)
                    }
                }

                Box::new(elaborated::ExprKind::Do(stmts))
            }
            (ExprKind::Cases(cases), _) => {
                let hole = ctx.hole(&env, Kind::typ());
                let hole_scrutinee = ctx.hole(&env, Kind::typ());

                let ambient = ctx.lacks(&env, Default::default());

                let ret_eff = ctx.lacks(&env, Default::default());

                for case in &cases.arms {
                    let mut env = env.clone();

                    if case.patterns.len() != 1 {
                        ctx.report(&env, TypeErrorKind::WrongArity(1, case.patterns.len()));
                        ctx.subsumes(env, ty, Type::error());
                        return Box::new(elaborated::ExprKind::Error);
                    }

                    let mut hashmap = Default::default();
                    let effect_pat = &EffectPat(hole_scrutinee.clone(), &case.patterns[0]);
                    let (pat_ty, _) = effect_pat.infer((ctx, &mut hashmap, &mut env));

                    let label = match pat_ty.deref().as_ref() {
                        TypeKind::Extend(l, eff, _) => {
                            ctx.subsumes(env.clone(), eff.clone(), ret_eff.clone());
                            l.clone()
                        }
                        _ => {
                            ctx.report(&env, TypeErrorKind::NotEffect);
                            ctx.subsumes(env, ty, Type::error());
                            return Box::new(elaborated::ExprKind::Error);
                        }
                    };

                    for binding in hashmap {
                        env.add_var(binding.0, binding.1);
                    }

                    case.expr.check(hole.clone(), (ctx, &ambient, env.clone()));

                    let request = ctx.find_prelude_type("Request", env.clone());

                    let app = Type::<Virtual>::application(
                        request,
                        vec![ret_eff.clone(), hole_scrutinee.clone()],
                    );

                    let fun = Type::<Virtual>::new(TypeKind::Arrow(r#virtual::Pi {
                        ty: app,
                        effs: ctx.remove_effect(label, &ambient),
                        body: hole.clone(),
                    }));

                    ctx.subsumes(env.clone(), fun, ty.clone());
                }

                Box::new(elaborated::ExprKind::Error)
            }
            (_, TypeKind::Forall(l)) => {
                let lvl_ty = Type::new(TypeKind::Bound(env.level));
                self.check(
                    l.body.apply_local(Some(l.name.clone()), lvl_ty.clone()),
                    (ctx, ambient, env.add(Some(l.name.clone()), lvl_ty)),
                )
            }
            _ => {
                let (expr_ty, elab_expr) = self.infer((ctx, ambient, env.clone()));
                ctx.subsumes(env, expr_ty, ty);
                elab_expr
            }
        }
    }
}

impl Check for Statement {
    type Return = (elaborated::Statement<Type<Real>>, Env);

    type Context<'a> = (&'a mut Context, &'a Effect<Virtual>, Env);

    fn check(&self, ty: Type<Virtual>, (ctx, ambient, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        let (typ, env, elab) = self.infer((ctx, ambient, env));

        ctx.subsumes(env.clone(), typ, ty);
        (elab, env)
    }
}
