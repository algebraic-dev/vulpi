use im_rc::HashMap;

use vulpi_syntax::r#abstract::{ExprKind, StatementKind};
use vulpi_syntax::{r#abstract::Expr, r#abstract::PatternArm};

use crate::ambient::Ambient;
use crate::infer::Infer;
use crate::kind;
use crate::{env::Env, types::Type};

use super::Check;

impl Check for PatternArm {
    type Return = Vec<Type>;

    type Context<'a> = (&'a mut Ambient, Env);

    fn check(&self, ty: Type, (ambient, mut context): Self::Context<'_>) -> Self::Return {
        let mut tys = Vec::new();

        let mut bindings = HashMap::new();

        for pat in &self.patterns {
            tys.push(pat.infer((context.clone(), &mut bindings)))
        }

        for binding in bindings {
            context.add_variable(binding.0, binding.1 .1)
        }

        if let Some(guard) = &self.guard {
            let Some(right) = context.import("Bool") else {
                return Vec::new()
            };

            let right = Type::variable(right);

            guard.check(right, (ambient, context.clone()));
        }

        self.expr.check(ty, (ambient, context));

        tys
    }
}

impl Check for (usize, &Vec<&PatternArm>) {
    type Return = Vec<Type>;

    type Context<'a> = (&'a mut Ambient, Env);

    fn check(&self, ty: Type, (ambient, context): Self::Context<'_>) -> Self::Return {
        let (size, arms) = self;
        let types = (0..*size)
            .map(|_| context.new_hole(kind::Kind::star()))
            .collect::<Vec<_>>();

        for arm in *arms {
            if arm.patterns.len() != *size {
                context.report(crate::error::TypeErrorKind::WrongArity(
                    *size,
                    arm.patterns.len(),
                ));
                return Vec::new();
            }

            let tys = arm.check(ty.clone(), (ambient, context.clone()));

            for (left, right) in types.iter().zip(tys.into_iter()) {
                left.sub(context.clone(), right);
            }
        }

        types
    }
}

impl Check for Expr {
    type Return = ();

    type Context<'a> = (&'a mut Ambient, Env);

    fn check(&self, ty: Type, (ambient, mut env): Self::Context<'_>) -> Self::Return {
        env.set_location(self.span.clone());

        match &self.data {
            ExprKind::Do(block) => {
                let Some(unit_qual) = env.import("Unit") else {
                    return;
                };

                let unit = Type::variable(unit_qual);

                if block.statements.is_empty() {
                    unit.sub(env.clone(), ty);
                    return;
                }

                for expr in block.statements.iter().take(block.statements.len() - 1) {
                    env.set_location(expr.span.clone());

                    match &expr.data {
                        StatementKind::Let(let_) => {
                            let mut bindings = HashMap::new();
                            let ty = let_.pattern.infer((env.clone(), &mut bindings));

                            let_.expr.check(ty.clone(), (ambient, env.clone()));

                            for (k, (_, t)) in bindings {
                                env.add_variable(k, t)
                            }
                        }
                        StatementKind::Expr(e) => {
                            e.infer((ambient, env.clone()));
                        }
                        StatementKind::Error => (),
                    };
                }

                let expr = block.statements.last().unwrap();

                env.set_location(expr.span.clone());

                match &expr.data {
                    StatementKind::Let(let_) => {
                        let mut bindings = HashMap::new();
                        let pat_ty = let_.pattern.infer((env.clone(), &mut bindings));
                        let_.expr.check(pat_ty, (ambient, env.clone()));
                        Type::unify(env.clone(), ty, unit);
                    }
                    StatementKind::Expr(e) => e.check(ty, (ambient, env.clone())),
                    StatementKind::Error => (),
                }
            }
            _ => {
                let infered = self.infer((ambient, env.clone()));
                ty.sub(env.clone(), infered);
            }
        }
    }
}
