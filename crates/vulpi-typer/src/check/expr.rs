use im_rc::HashMap;

use vulpi_syntax::r#abstract::{ExprKind, StatementKind};
use vulpi_syntax::{r#abstract::Expr, r#abstract::PatternArm};

use crate::infer::Infer;
use crate::{env::Env, types::Type};

use super::Check;

impl Check for PatternArm {
    type Return = Vec<Type>;

    type Context<'a> = Env;

    fn check(&self, ty: Type, mut context: Self::Context<'_>) -> Self::Return {
        let mut tys = Vec::new();

        let mut bindings = HashMap::new();

        for pat in &self.patterns {
            tys.push(pat.infer((context.clone(), &mut bindings)))
        }

        for binding in bindings {
            context.add_variable(binding.0, binding.1 .1)
        }

        if let Some(ty) = self.guard.infer(context.clone()) {
            let Some(right) = context.import("Bool") else {
                return Vec::new()
            };

            let right = Type::variable(right);

            Type::unify(context.clone(), ty, right);
        }

        self.expr.check(ty, context);

        tys
    }
}

impl Check for (usize, &Vec<&PatternArm>) {
    type Return = Vec<Type>;

    type Context<'a> = Env;

    fn check(&self, ty: Type, context: Self::Context<'_>) -> Self::Return {
        let (size, arms) = self;
        let types = (0..*size).map(|_| context.new_hole()).collect::<Vec<_>>();

        for arm in *arms {
            if arm.patterns.len() != *size {
                context.report(crate::error::TypeErrorKind::WrongArity(
                    *size,
                    arm.patterns.len(),
                ));
                return Vec::new();
            }

            let tys = arm.check(ty.clone(), context.clone());

            for (left, right) in types.iter().zip(tys.into_iter()) {
                Type::unify(context.clone(), left.clone(), right);
            }
        }

        types
    }
}

impl Check for Expr {
    type Return = ();

    type Context<'a> = Env;

    fn check(&self, ty: Type, mut context: Self::Context<'_>) -> Self::Return {
        context.set_location(self.span.clone());

        match &self.data {
            ExprKind::Do(block) => {
                let Some(unit_qual) = context.import("Unit") else {
                    return;
                };

                let unit = Type::variable(unit_qual);

                if block.statements.is_empty() {
                    Type::unify(context.clone(), unit, ty);
                    return;
                }

                for expr in block.statements.iter().take(block.statements.len() - 1) {
                    context.set_location(expr.span.clone());

                    match &expr.data {
                        StatementKind::Let(let_) => {
                            let mut bindings = HashMap::new();
                            let ty = let_.pattern.infer((context.clone(), &mut bindings));

                            let body = let_.expr.infer(context.clone());
                            Type::unify(context.clone(), ty, body);

                            for (k, (_, t)) in bindings {
                                context.add_variable(k, t)
                            }
                        }
                        StatementKind::Expr(e) => {
                            e.infer(context.clone());
                        }
                        StatementKind::Error => (),
                    };
                }

                let expr = block.statements.last().unwrap();

                context.set_location(expr.span.clone());

                match &expr.data {
                    StatementKind::Let(let_) => {
                        let mut bindings = HashMap::new();
                        let pat_ty = let_.pattern.infer((context.clone(), &mut bindings));
                        let_.expr.check(pat_ty, context.clone());
                        Type::unify(context.clone(), ty, unit);
                    }
                    StatementKind::Expr(e) => e.check(ty, context.clone()),
                    StatementKind::Error => (),
                }
            }
            _ => {
                let infered = self.infer(context.clone());
                Type::unify(context.clone(), infered, ty);
            }
        }
    }
}
