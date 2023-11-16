//! Inference of patterns

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::{
    elaborated::{self, PatApplication},
    r#abstract::Pattern,
    r#abstract::PatternArm,
    r#abstract::PatternKind,
};

use crate::{
    errors::TypeErrorKind,
    r#type::{eval::Eval, r#virtual::Virtual},
    Context, Env, Kind, Real, Type,
};

use super::Infer;

impl Infer for PatternArm {
    type Return = (
        Vec<Type<Virtual>>,
        Type<Virtual>,
        elaborated::PatternArm<Type<Real>>,
    );

    type Context<'a> = (&'a mut Context, Env);

    fn infer(&self, (ctx, mut env): Self::Context<'_>) -> Self::Return {
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

        let (typ, elab_expr) = self.expr.infer((ctx, env.clone()));

        let guard = self
            .guard
            .as_ref()
            .map(|g| g.infer((ctx, env.clone())));

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
        Vec<elaborated::PatternArm<Type<Real>>>,
    );

    type Context<'a> = (&'a mut Context, Env);

    fn infer(&self, (ctx, env): Self::Context<'_>) -> Self::Return {
        if self.is_empty() {
            (
                ctx.hole(&env, Kind::typ()),
                vec![],
                ctx.hole(&env, Kind::typ()),
                vec![],
            )
        } else {
            let (types, ret_type, elab_arm) = self[0].infer((ctx, env.clone()));

            let mut elab_arms = vec![elab_arm];

            for pat in self.iter().skip(1) {
                let (new_types, new_ret_type, elab_arm) =
                    pat.infer((ctx, env.clone()));

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
            PatternKind::Tuple(tuple) => {
                let mut types = Vec::new();
                let mut elab_pats = Vec::new();

                for pat in tuple {
                    let (typ, elab_pat) = pat.infer((ctx, map, env.clone()));
                    types.push(typ);
                    elab_pats.push(elab_pat);
                }

                (
                    Type::tuple(types),
                    Box::new(elaborated::PatternKind::Tuple(elab_pats)),
                )
            }
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
            PatternKind::Or(_) => {
                unimplemented!("Or patterns are not yet implemented")
            }
            PatternKind::Application(app) => {
                let (typ, arity, _) = ctx.modules.constructor(&app.func);

                let mut typ = typ.eval(&env);

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

                    let Some((param_ty, rest)) = ctx.as_function(&env, typ) else { unreachable!() };

                    typ = rest;

                    ctx.subsumes(env.clone(), arg_ty, param_ty);
                }

                (
                    typ,
                    Box::new(elaborated::PatternKind::Application(PatApplication {
                        func: app.func.clone(),
                        args,
                    })),
                )
            }
            PatternKind::Error => (Type::error(), Box::new(elaborated::PatternKind::Error)),
        }
    }
}