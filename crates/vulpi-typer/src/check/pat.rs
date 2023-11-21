use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::{
    elaborated,
    r#abstract::{Pattern, PatternArm, PatternKind},
};

use crate::eval::Quote;
use crate::infer::Infer;
use crate::{context::Context, errors::TypeErrorKind, r#virtual::Virtual, real::Real, Env, Type};

use super::Check;

impl Check for PatternArm {
    type Return = elaborated::PatternArm<Type<Real>>;

    type Context<'a> = (&'a mut Context, Env);

    fn check(&self, mut typ: Type<Virtual>, (ctx, mut env): Self::Context<'_>) -> Self::Return {
        let mut map = Default::default();

        let mut elaborated_patterns = Vec::new();

        for pat in &self.patterns {
            env.set_current_span(pat.span.clone());

            if let Some((left, right)) = ctx.as_function(&env, typ.clone()) {
                let elab = pat.check(left, (ctx, &mut map, env.clone()));
                elaborated_patterns.push(elab);
                typ = right;
            } else {
                ctx.report(
                    &env,
                    TypeErrorKind::NotAFunction(env.clone(), typ.quote(env.level)),
                );
                return elaborated::PatternArm {
                    patterns: Vec::new(),
                    guard: None,
                    expr: self.expr.check(typ, (ctx, env.clone())),
                };
            }
        }

        for binding in map {
            env.add_var(binding.0, binding.1);
        }

        let elab_expr = self.expr.check(typ, (ctx, env.clone()));

        let guard = self.guard.as_ref().map(|g| g.infer((ctx, env.clone())));

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

    type Context<'a> = (&'a mut Context, Env);

    fn check(&self, typ: Type<Virtual>, (ctx, env): Self::Context<'_>) -> Self::Return {
        if self.is_empty() {
            ctx.report(&env, TypeErrorKind::EmptyCase);
            vec![]
        } else {
            let size = self[0].patterns.len();

            let mut elab_arms = Vec::new();
            let elab_arm = self[0].check(typ.clone(), (ctx, env.clone()));

            elab_arms.push(elab_arm);

            for pat in self.iter().skip(1) {
                if pat.patterns.len() != size {
                    ctx.report(&env, TypeErrorKind::WrongArity(pat.patterns.len(), size));
                    return vec![];
                }

                let elab_arm = pat.check(typ.clone(), (ctx, env.clone()));
                elab_arms.push(elab_arm);
            }

            elab_arms
        }
    }
}

impl Check for Pattern {
    type Return = elaborated::Pattern;

    type Context<'a> = (&'a mut Context, &'a mut HashMap<Symbol, Type<Virtual>>, Env);

    fn check(&self, ann_ty: Type<Virtual>, (ctx, map, env): Self::Context<'_>) -> Self::Return {
        env.set_current_span(self.span.clone());
        match &self.data {
            PatternKind::Wildcard => Box::new(elaborated::PatternKind::Wildcard),
            PatternKind::Variable(n) => {
                map.insert(n.clone(), ann_ty);

                Box::new(elaborated::PatternKind::Variable(n.clone()))
            }
            _ => {
                let (typ, elab_pat) = self.infer((ctx, map, env.clone()));
                ctx.subsumes(env, typ, ann_ty);
                elab_pat
            }
        }
    }
}
