use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::{r#abstract::Pattern, r#abstract::PatternArm};

use crate::{
    errors::TypeErrorKind,
    infer::pat::EffectPat,
    r#type::{eval::Quote, Effect},
    Context, Env, Type, Virtual,
};

use super::Check;
use crate::infer::Infer;

impl Check for PatternArm {
    type Return = ();

    type Context<'a> = (&'a mut Context, Effect<Virtual>, Env);

    fn check(
        &self,
        mut ty: Type<Virtual>,
        (ctx, ambient, mut env): Self::Context<'_>,
    ) -> Self::Return {
        let mut map = Default::default();

        for pat in &self.patterns {
            env.on(pat.span.clone());

            if let Some((left, _, right)) = ctx.as_function(&env, ty.clone()) {
                pat.check(left, (ctx, &mut map, env.clone()));
                ty = right;
            } else {
                ctx.report(
                    &env,
                    TypeErrorKind::NotAFunction(env.clone(), ty.quote(env.level)),
                );
                return;
            }
        }

        for binding in map {
            env.add_var(binding.0, binding.1);
        }

        self.expr.check(ty, (ctx, &ambient, env.clone()));
    }
}

impl Check for Vec<PatternArm> {
    type Return = ();

    type Context<'a> = (&'a mut Context, Effect<Virtual>, Env);

    fn check(&self, ty: Type<Virtual>, (ctx, ambient, env): Self::Context<'_>) -> Self::Return {
        if self.is_empty() {
            todo!()
        } else {
            let size = self[0].patterns.len();

            self[0].check(ty.clone(), (ctx, ambient.clone(), env.clone()));

            for pat in self.iter().skip(1) {
                if pat.patterns.len() != size {
                    ctx.report(&env, TypeErrorKind::WrongArity(pat.patterns.len(), size));
                    return;
                }

                pat.check(ty.clone(), (ctx, ambient.clone(), env.clone()));
            }
        }
    }
}

impl Check for Pattern {
    type Return = ();

    type Context<'a> = (&'a mut Context, &'a mut HashMap<Symbol, Type<Virtual>>, Env);

    fn check(&self, ty: Type<Virtual>, (ctx, map, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());
        let infered = self.infer((ctx, map, env.clone()));
        ctx.subsumes(env, infered, ty);
    }
}

impl<'b> Check for EffectPat<'b> {
    type Return = ();

    type Context<'a> = (&'a mut Context, &'a mut HashMap<Symbol, Type<Virtual>>, Env);

    fn check(&self, ty: Type<Virtual>, context: Self::Context<'_>) -> Self::Return {
        let (ctx, map, env) = context;
        env.on(self.1.span.clone());
        let infered = self.1.infer((ctx, map, env.clone()));
        ctx.subsumes(env, infered, ty);
    }
}
