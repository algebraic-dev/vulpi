//! Checking of expressions

use vulpi_syntax::{r#abstract::Expr, r#abstract::ExprKind};

use crate::r#type::eval::Quote;
use crate::r#type::real;
use crate::Real;
use crate::{
    errors::TypeErrorKind,
    infer::{literal::find_prelude_type, pat::EffectPat},
    r#type::{
        r#virtual::{self, Closure},
        Effect, Index, TypeKind,
    },
    Context, Env, Kind, Type, Virtual,
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
            (ExprKind::Cases(cases), _) => {
                let name = ctx.new_name();
                let env = env.add(Some(name), ctx.hole(&env, Kind::typ()));

                let hole = ctx.hole(&env, Kind::typ());
                let hole_scrutinee = ctx.hole(&env, Kind::typ());

                let ambient = ctx.lacks(&env, Default::default());

                let ret_eff = ctx.lacks(&env, Default::default());

                for case in &cases.arms {
                    let mut env = env.clone();

                    if case.patterns.len() != 1 {
                        ctx.report(&env, TypeErrorKind::WrongArity(1, case.patterns.len()));
                        ctx.subsumes(env, ty, Type::error());
                        return;
                    }

                    let mut hashmap = Default::default();
                    let effect_pat = &EffectPat(hole_scrutinee.clone(), &case.patterns[0]);
                    let pat_ty = effect_pat.infer((ctx, &mut hashmap, &mut env));

                    match pat_ty.deref().as_ref() {
                        TypeKind::Extend(_, eff, _) => {
                            ctx.subsumes(env.clone(), eff.clone(), ret_eff.clone());
                            eff
                        }
                        _ => {
                            ctx.report(&env, TypeErrorKind::NotEffect);
                            ctx.subsumes(env, ty, Type::error());
                            return;
                        }
                    };

                    for binding in hashmap {
                        env.add_var(binding.0, binding.1);
                    }

                    case.expr.check(hole.clone(), (ctx, &ambient, env.clone()));

                    let request = find_prelude_type("Request", ctx, env.clone());

                    let app = Type::<Virtual>::application(
                        request,
                        vec![ret_eff.clone(), hole_scrutinee.clone()],
                    );

                    let fun = Type::<Virtual>::new(TypeKind::Arrow(r#virtual::Pi {
                        ty: app,
                        effs: ambient.clone(),
                        body: hole.clone(),
                    }));

                    ctx.subsumes(env, fun, ty.clone());
                }
            }
            _ => {
                let expr_ty = self.infer((ctx, ambient, env.clone()));
                ctx.subsumes(env, expr_ty, ty);
            }
        }
    }
}
