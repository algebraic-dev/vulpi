//! Inference of patterns

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::{
    elaborated::{self, PatApplication, PatOr},
    r#abstract::Pattern,
    r#abstract::PatternArm,
    r#abstract::PatternKind,
};

use crate::{
    errors::TypeErrorKind,
    r#type::{eval::Eval, r#virtual::Virtual, Effect},
    Context, Env, Kind, Type,
};

use super::Infer;

impl Infer for PatternArm {
    type Return = (
        Vec<Type<Virtual>>,
        Type<Virtual>,
        elaborated::PatternArm<Type<Virtual>>,
    );

    type Context<'a> = (&'a mut Context, Effect<Virtual>, Env);

    fn infer(&self, (ctx, ambient, mut env): Self::Context<'_>) -> Self::Return {
        let mut patterns = Vec::new();
        let mut elaborated_patterns = Vec::new();

        let mut map = Default::default();
        for pat in &self.patterns {
            let (typ, elab) = pat.infer((ctx, &mut map, env.clone()));
            patterns.push(typ);
            elaborated_patterns.push(elab);
        }

        for binding in map {
            env.add_var(binding.0, binding.1);
        }

        let (typ, elab_expr) = self.expr.infer((ctx, &ambient, env.clone()));

        let guard = self
            .guard
            .as_ref()
            .map(|g| g.infer((ctx, &ambient, env.clone())));

        let elab_guard = if let Some((typ, guard)) = guard {
            let bool = ctx.find_prelude_type("Bool", env.clone());
            ctx.subsumes(env.clone(), typ, bool);
            Some(guard)
        } else {
            None
        };

        (
            patterns,
            typ,
            elaborated::PatternArm {
                patterns: elaborated_patterns,
                guard: elab_guard,
                expr: elab_expr,
            },
        )
    }
}

impl Infer for Vec<PatternArm> {
    type Return = (
        Type<Virtual>,
        Vec<Type<Virtual>>,
        Type<Virtual>,
        Vec<elaborated::PatternArm<Type<Virtual>>>,
    );

    type Context<'a> = (&'a mut Context, Effect<Virtual>, Env);

    fn infer(&self, (ctx, ambient, env): Self::Context<'_>) -> Self::Return {
        if self.is_empty() {
            (
                ctx.hole(&env, Kind::typ()),
                vec![],
                ctx.hole(&env, Kind::typ()),
                vec![],
            )
        } else {
            let (types, ret_type, elab_arm) = self[0].infer((ctx, ambient.clone(), env.clone()));

            let mut elab_arms = vec![elab_arm];

            for pat in self.iter().skip(1) {
                let (new_types, new_ret_type, elab_arm) =
                    pat.infer((ctx, ambient.clone(), env.clone()));

                elab_arms.push(elab_arm);

                if new_types.len() != types.len() {
                    ctx.report(
                        &env,
                        TypeErrorKind::WrongArity(new_types.len(), types.len()),
                    );
                    return (Type::error(), types, Type::error(), vec![]);
                }

                for (old, new) in types.iter().zip(new_types) {
                    ctx.subsumes(env.clone(), old.clone(), new);
                }

                ctx.subsumes(env.clone(), ret_type.clone(), new_ret_type);
            }

            (
                Type::<Virtual>::function(types.clone(), ret_type.clone()),
                types,
                ret_type,
                elab_arms,
            )
        }
    }
}
/// Helper structure made for inference of effect patterns.
pub(crate) struct EffectPat<'a>(pub Type<Virtual>, pub &'a Pattern);

impl Infer for Pattern {
    type Return = (Type<Virtual>, elaborated::Pattern);

    type Context<'a> = (&'a mut Context, &'a mut HashMap<Symbol, Type<Virtual>>, Env);

    fn infer(&self, (ctx, map, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        match &self.data {
            PatternKind::Wildcard => (
                ctx.hole(&env, Type::typ()),
                Box::new(elaborated::PatternKind::Wildcard),
            ),
            PatternKind::Variable(symbol) => {
                let value = ctx.hole(&env, Type::typ());

                if let Some(typ) = map.get(symbol) {
                    ctx.subsumes(env, typ.clone(), value.clone());
                } else {
                    map.insert(symbol.clone(), value.clone());
                }

                (
                    value,
                    Box::new(elaborated::PatternKind::Variable(symbol.clone())),
                )
            }
            PatternKind::Literal(lit) => {
                let (typ, lit) = lit.infer((ctx, env));
                (typ, Box::new(elaborated::PatternKind::Literal(lit)))
            }
            PatternKind::Ascription(ann) => {
                let (typ, _) = ann.typ.infer((ctx, env.clone()));
                let eval_typ = typ.eval(&env);
                let (value, pat) = ann.pat.infer((ctx, map, env.clone()));
                ctx.subsumes(env, eval_typ.clone(), value);
                (eval_typ, pat)
            }
            PatternKind::Or(or) => {
                let (left, left_pat) = or.left.infer((ctx, map, env.clone()));
                let (right, right_pat) = or.right.infer((ctx, map, env.clone()));
                ctx.subsumes(env, left.clone(), right);
                (
                    left,
                    Box::new(elaborated::PatternKind::Or(PatOr {
                        left: left_pat,
                        right: right_pat,
                    })),
                )
            }
            PatternKind::Application(app) => {
                let (mut typ, arity) = ctx.modules.constructor(&app.func);

                if arity != app.args.len() {
                    ctx.report(&env, TypeErrorKind::WrongArity(arity, app.args.len()));
                    return (Type::error(), Box::new(elaborated::PatternKind::Error));
                }

                let mut types = Vec::new();
                let mut args = Vec::new();

                for arg in &app.args {
                    let (arg_ty, elab_arg) = arg.infer((ctx, map, env.clone()));

                    types.push(arg_ty.clone());
                    args.push(elab_arg);

                    let Some((param_ty, _, rest)) = ctx.as_function(&env, typ) else { unreachable!() };

                    typ = rest;
                    ctx.subsumes(env.clone(), param_ty, arg_ty);
                }

                (
                    typ,
                    Box::new(elaborated::PatternKind::Application(PatApplication {
                        func: app.func.clone(),
                        args,
                    })),
                )
            }
            PatternKind::Effect(_) => {
                ctx.report(&env, TypeErrorKind::EffectsNotAllowedInNormalPatterns);
                (Type::error(), Box::new(elaborated::PatternKind::Error))
            }
            PatternKind::Error => (Type::error(), Box::new(elaborated::PatternKind::Error)),
        }
    }
}

impl<'b> Infer for EffectPat<'b> {
    type Return = (Type<Virtual>, elaborated::PatEffectKind);

    type Context<'a> = (
        &'a mut Context,
        &'a mut HashMap<Symbol, Type<Virtual>>,
        &'a mut Env,
    );

    fn infer(&self, (ctx, map, env): Self::Context<'_>) -> Self::Return {
        env.on(self.1.span.clone());

        match &self.1.data {
            PatternKind::Wildcard => (
                ctx.lacks(env, Default::default()),
                elaborated::PatEffectKind::Wildcard,
            ),
            PatternKind::Variable(n) => {
                map.insert(n.clone(), self.0.clone());
                (
                    ctx.lacks(env, Default::default()),
                    elaborated::PatEffectKind::Variable(n.clone()),
                )
            }
            PatternKind::Effect(eff) => {
                let (mut typ, arity) = ctx.modules.effect(&eff.func);

                if arity != eff.args.len() {
                    ctx.report(env, TypeErrorKind::WrongArity(arity, eff.args.len()));
                    return (Type::error(), elaborated::PatEffectKind::Error);
                }

                let mut types = Vec::new();
                let mut ret_eff = typ.clone();
                let mut elab_args = Vec::new();

                for arg in &eff.args {
                    let (arg_ty, elab_arg) = arg.infer((ctx, map, env.clone()));
                    types.push(arg_ty.clone());
                    elab_args.push(elab_arg);

                    let Some((param_ty, effect, rest)) = ctx.as_function(env, typ) else { unreachable!() };
                    ret_eff = effect;

                    typ = rest;
                    ctx.subsumes(env.clone(), param_ty, arg_ty);
                }

                if let Some(cont) = &eff.cont {
                    let cont_ty = Type::<Virtual>::function(vec![typ], self.0.clone());
                    map.insert(cont.clone(), cont_ty);
                }

                (
                    ret_eff,
                    elaborated::PatEffectKind::Effect(elaborated::PatEffect {
                        func: eff.func.clone(),
                        args: elab_args,
                        cont: eff.cont.clone(),
                    }),
                )
            }
            PatternKind::Error => (Type::error(), elaborated::PatEffectKind::Error),
            _ => {
                ctx.report(env, TypeErrorKind::EffectsNotAllowedInNormalPatterns);
                (Type::error(), elaborated::PatEffectKind::Error)
            }
        }
    }
}
