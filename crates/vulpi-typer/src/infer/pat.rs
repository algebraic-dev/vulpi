//! Inference of patterns

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::{r#abstract::Pattern, r#abstract::PatternArm, r#abstract::PatternKind};

use crate::{
    errors::TypeErrorKind,
    r#type::{
        eval::{Eval, Quote},
        r#virtual::Virtual,
        Effect,
    },
    Context, Env, Kind, Type,
};

use super::Infer;

impl Infer for PatternArm {
    type Return = (Vec<Type<Virtual>>, Type<Virtual>);

    type Context<'a> = (&'a mut Context, Effect<Virtual>, Env);

    fn infer(&self, (ctx, ambient, mut env): Self::Context<'_>) -> Self::Return {
        let mut patterns = Vec::new();

        let mut map = Default::default();
        for pat in &self.patterns {
            let typ = pat.infer((ctx, &mut map, env.clone()));
            patterns.push(typ);
        }

        for binding in map {
            env.add_var(binding.0, binding.1);
        }

        let typ = self.expr.infer((ctx, &ambient, env.clone()));

        (patterns, typ)
    }
}

impl Infer for Vec<PatternArm> {
    type Return = (Type<Virtual>, Vec<Type<Virtual>>, Type<Virtual>);

    type Context<'a> = (&'a mut Context, Effect<Virtual>, Env);

    fn infer(&self, (ctx, ambient, env): Self::Context<'_>) -> Self::Return {
        if self.is_empty() {
            (
                ctx.hole(&env, Kind::typ()),
                vec![],
                ctx.hole(&env, Kind::typ()),
            )
        } else {
            let (types, ret_type) = self[0].infer((ctx, ambient.clone(), env.clone()));

            for pat in self.iter().skip(1) {
                let (new_types, new_ret_type) = pat.infer((ctx, ambient.clone(), env.clone()));

                if new_types.len() != types.len() {
                    ctx.report(
                        &env,
                        TypeErrorKind::WrongArity(new_types.len(), types.len()),
                    );
                    return (Type::error(), types, Type::error());
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
            )
        }
    }
}
/// Helper structure made for inference of effect patterns.
pub(crate) struct EffectPat<'a>(pub Type<Virtual>, pub &'a Pattern);

impl Infer for Pattern {
    type Return = Type<Virtual>;

    type Context<'a> = (&'a mut Context, &'a mut HashMap<Symbol, Type<Virtual>>, Env);

    fn infer(&self, (ctx, map, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        match &self.data {
            PatternKind::Wildcard => ctx.hole(&env, Type::typ()),
            PatternKind::Variable(symbol) => {
                let value = ctx.hole(&env, Type::typ());

                if let Some(typ) = map.get(symbol) {
                    ctx.subsumes(env, typ.clone(), value.clone());
                } else {
                    map.insert(symbol.clone(), value.clone());
                }

                value
            }
            PatternKind::Literal(lit) => lit.infer((ctx, env)),
            PatternKind::Ascription(ann) => {
                let (typ, _) = ann.typ.infer((ctx, env.clone()));
                let eval_typ = typ.eval(&env);
                let value = ann.pat.infer((ctx, map, env.clone()));
                ctx.subsumes(env, eval_typ.clone(), value);
                eval_typ
            }
            PatternKind::Or(or) => {
                let left = or.left.infer((ctx, map, env.clone()));
                let right = or.right.infer((ctx, map, env.clone()));
                ctx.subsumes(env, left.clone(), right);
                left
            }
            PatternKind::Application(app) => {
                let (mut typ, arity) = ctx.modules.constructor(&app.func);

                if arity != app.args.len() {
                    ctx.report(&env, TypeErrorKind::WrongArity(arity, app.args.len()));
                    return Type::error();
                }

                let mut types = Vec::new();

                for arg in &app.args {
                    let arg_ty = arg.infer((ctx, map, env.clone()));
                    types.push(arg_ty.clone());

                    let Some((param_ty, _, rest)) = ctx.as_function(&env, typ) else { unreachable!() };

                    typ = rest;
                    ctx.subsumes(env.clone(), param_ty, arg_ty);
                }

                typ
            }
            PatternKind::Effect(_) => {
                ctx.report(&env, TypeErrorKind::EffectsNotAllowedInNormalPatterns);
                Type::error()
            }
            PatternKind::Error => Type::error(),
        }
    }
}

impl<'b> Infer for EffectPat<'b> {
    type Return = Type<Virtual>;

    type Context<'a> = (
        &'a mut Context,
        &'a mut HashMap<Symbol, Type<Virtual>>,
        &'a mut Env,
    );

    fn infer(&self, (ctx, map, mut env): Self::Context<'_>) -> Self::Return {
        env.on(self.1.span.clone());

        match &self.1.data {
            PatternKind::Wildcard => ctx.lacks(&env, Default::default()),
            PatternKind::Variable(n) => {
                map.insert(n.clone(), self.0.clone());
                ctx.lacks(&env, Default::default())
            }
            PatternKind::Effect(eff) => {
                let (mut typ, arity) = ctx.modules.effect(&eff.func);

                if arity != eff.args.len() {
                    ctx.report(env, TypeErrorKind::WrongArity(arity, eff.args.len()));
                    return Type::error();
                }

                let mut types = Vec::new();
                let mut ret_eff = typ.clone();

                for arg in &eff.args {
                    let arg_ty = arg.infer((ctx, map, env.clone()));
                    types.push(arg_ty.clone());

                    let Some((param_ty, effect, rest)) = ctx.as_function(&env, typ) else { unreachable!() };
                    ret_eff = effect;

                    typ = rest;
                    ctx.subsumes(env.clone(), param_ty, arg_ty);
                }

                if let Some(cont) = &eff.cont {
                    let cont_ty = Type::<Virtual>::function(vec![typ], self.0.clone());
                    //let cont_ty = ctx.skolemize(env, &cont_ty);
                    map.insert(cont.clone(), cont_ty);
                }

                ret_eff
            }
            PatternKind::Error => Type::error(),
            _ => {
                ctx.report(&env, TypeErrorKind::EffectsNotAllowedInNormalPatterns);
                Type::error()
            }
        }
    }
}
