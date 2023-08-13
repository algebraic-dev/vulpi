//! Checking of expressions

use vulpi_syntax::{elaborated, r#abstract::Expr, r#abstract::ExprKind};

use crate::{
    errors::TypeErrorKind,
    infer::pat::EffectPat,
    r#type::{r#virtual, Effect, TypeKind},
    Context, Env, Kind, Type, Virtual,
};

use super::Check;
use crate::infer::Infer;

impl Check for Expr {
    type Return = elaborated::Expr<Type<Virtual>>;

    type Context<'a> = (&'a mut Context, &'a Effect<Virtual>, Env);

    fn check(
        &self,
        ty: crate::Type<crate::Virtual>,
        (ctx, ambient, env): Self::Context<'_>,
    ) -> Self::Return {
        env.on(self.span.clone());
        match (&self.data, ty.deref().as_ref()) {
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

                    match pat_ty.deref().as_ref() {
                        TypeKind::Extend(_, eff, _) => {
                            ctx.subsumes(env.clone(), eff.clone(), ret_eff.clone());
                            eff
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
                        effs: ambient.clone(),
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
