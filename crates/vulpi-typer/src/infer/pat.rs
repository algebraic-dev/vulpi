//! Inference of patterns

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::{r#abstract::Pattern, r#abstract::PatternKind};

use crate::{
    errors::TypeErrorKind,
    r#type::{eval::Eval, r#virtual::Virtual},
    Context, Env, Type,
};

use super::Infer;

pub struct EffectPat(pub Pattern);

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

impl Infer for EffectPat {
    type Return = Type<Virtual>;

    type Context<'a> = (&'a mut Context, &'a mut HashMap<Symbol, Type<Virtual>>, Env);

    fn infer(&self, (ctx, map, env): Self::Context<'_>) -> Self::Return {
        env.on(self.0.span.clone());

        match &self.0.data {
            PatternKind::Wildcard => ctx.hole(&env, Type::typ()),
            PatternKind::Variable(x) => todo!(),
            PatternKind::Effect(eff) => {
                let (mut typ, arity) = ctx.modules.effect(&eff.func);

                if arity != eff.args.len() {
                    ctx.report(&env, TypeErrorKind::WrongArity(arity, eff.args.len()));
                    return Type::error();
                }

                let mut types = Vec::new();

                for arg in &eff.args {
                    let arg_ty = arg.infer((ctx, map, env.clone()));
                    types.push(arg_ty.clone());

                    let Some((param_ty, _, rest)) = ctx.as_function(&env, typ) else { unreachable!() };

                    typ = rest;
                    ctx.subsumes(env.clone(), param_ty, arg_ty);
                }

                typ
            }
            PatternKind::Error => Type::error(),
            _ => {
                ctx.report(&env, TypeErrorKind::EffectsNotAllowedInNormalPatterns);
                Type::error()
            }
        }
    }
}
