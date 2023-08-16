use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::{
    elaborated, r#abstract::Pattern, r#abstract::PatternArm, r#abstract::PatternKind,
};

use crate::{
    errors::TypeErrorKind,
    infer::pat::EffectPat,
    r#type::{eval::Quote, Effect},
    Context, Env, Real, Type, Virtual,
};

use super::Check;
use crate::infer::Infer;

impl Check for PatternArm {
    type Return = elaborated::PatternArm<Type<Real>>;

    type Context<'a> = (&'a mut Context, Effect<Virtual>, Env);

    fn check(
        &self,
        mut ty: Type<Virtual>,
        (ctx, ambient, mut env): Self::Context<'_>,
    ) -> Self::Return {
        let mut map = Default::default();

        let mut elaborated_patterns = Vec::new();

        for pat in &self.patterns {
            env.on(pat.span.clone());

            if let Some((left, _, right)) = ctx.as_function(&env, ty.clone()) {
                let elab = pat.check(left, (ctx, &mut map, env.clone()));
                elaborated_patterns.push(elab);
                ty = right;
            } else {
                ctx.report(
                    &env,
                    TypeErrorKind::NotAFunction(env.clone(), ty.quote(env.level)),
                );
                return elaborated::PatternArm {
                    patterns: Vec::new(),
                    guard: None,
                    expr: self.expr.check(ty, (ctx, &ambient, env.clone())),
                };
            }
        }

        for binding in map {
            env.add_var(binding.0, binding.1);
        }

        let elab_expr = self.expr.check(ty, (ctx, &ambient, env.clone()));

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

        elaborated::PatternArm {
            patterns: elaborated_patterns,
            guard: elab_guard,
            expr: elab_expr,
        }
    }
}

impl Check for Vec<PatternArm> {
    type Return = Vec<elaborated::PatternArm<Type<Real>>>;

    type Context<'a> = (&'a mut Context, Effect<Virtual>, Env);

    fn check(&self, ty: Type<Virtual>, (ctx, ambient, env): Self::Context<'_>) -> Self::Return {
        if self.is_empty() {
            todo!()
        } else {
            let size = self[0].patterns.len();

            let mut elab_arms = Vec::new();
            let elab_arm = self[0].check(ty.clone(), (ctx, ambient.clone(), env.clone()));

            elab_arms.push(elab_arm);

            for pat in self.iter().skip(1) {
                if pat.patterns.len() != size {
                    ctx.report(&env, TypeErrorKind::WrongArity(pat.patterns.len(), size));
                    return vec![];
                }

                let elab_arm = pat.check(ty.clone(), (ctx, ambient.clone(), env.clone()));
                elab_arms.push(elab_arm);
            }

            elab_arms
        }
    }
}

impl Check for Pattern {
    type Return = elaborated::Pattern;

    type Context<'a> = (&'a mut Context, &'a mut HashMap<Symbol, Type<Virtual>>, Env);

    fn check(&self, ty: Type<Virtual>, (ctx, map, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());
        match &self.data {
            PatternKind::Wildcard => Box::new(elaborated::PatternKind::Wildcard),
            PatternKind::Variable(n) => {
                map.insert(n.clone(), ty);

                Box::new(elaborated::PatternKind::Variable(n.clone()))
            }
            _ => {
                let (typ, elab_pat) = self.infer((ctx, map, env.clone()));
                ctx.subsumes(env, typ, ty);
                elab_pat
            }
        }
    }
}

impl<'b> Check for EffectPat<'b> {
    type Return = elaborated::PatEffectKind;

    type Context<'a> = (&'a mut Context, &'a mut HashMap<Symbol, Type<Virtual>>, Env);

    fn check(&self, ty: Type<Virtual>, context: Self::Context<'_>) -> Self::Return {
        let (ctx, map, env) = context;
        env.on(self.1.span.clone());
        let (infered, pat) = self.infer((ctx, map, &mut env.clone()));
        ctx.subsumes(env, infered, ty);
        pat
    }
}
