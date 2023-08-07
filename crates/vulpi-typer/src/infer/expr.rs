use std::collections::HashSet;

use crate::ambient::Ambient;
use crate::apply::Apply;
use crate::check::Check;
use crate::kind::Kind;
use crate::module::Def;
use crate::types::TypeKind;
use crate::{env::Env, types::Type, Infer};

use im_rc::HashMap;
use vulpi_intern::Symbol;
use vulpi_syntax::{
    r#abstract::Expr, r#abstract::ExprKind, r#abstract::PatternArm, r#abstract::StatementKind,
};

impl Infer for PatternArm {
    type Return = (Vec<Type>, Type);

    type Context<'a> = (&'a mut Ambient, Env);

    fn infer(&self, (ambient, mut env): Self::Context<'_>) -> Self::Return {
        let mut tys = Vec::new();

        let mut bindings = HashMap::new();

        for pat in &self.patterns {
            tys.push(pat.infer((env.clone(), &mut bindings)))
        }

        for binding in bindings {
            env.add_variable(binding.0, binding.1 .1)
        }

        if let Some(guard) = &self.guard {
            let right = env.import("Bool").unwrap();
            let right = Type::variable(right);

            guard.check(right, (ambient, env.clone()));
        }

        let result = self.expr.infer((ambient, env));

        (tys, result)
    }
}

impl Infer for (usize, &Vec<&PatternArm>) {
    type Return = (Vec<Type>, Type);

    type Context<'a> = (&'a mut Ambient, Env);

    fn infer(&self, (ambient, context): Self::Context<'_>) -> Self::Return {
        let (size, arms) = self;
        let types = (0..*size)
            .map(|_| context.new_hole(Kind::star()))
            .collect::<Vec<_>>();
        let ret = context.new_hole(Kind::star());

        for arm in *arms {
            if arm.patterns.len() != *size {
                context.report(crate::error::TypeErrorKind::WrongArity(
                    *size,
                    arm.patterns.len(),
                ));
                return (Vec::new(), Type::error());
            }

            let tys = arm.check(ret.clone(), (ambient, context.clone()));

            for (left, right) in types.iter().zip(tys.into_iter()) {
                left.sub(context.clone(), right);
            }
        }

        (types, ret)
    }
}

impl Infer for Expr {
    type Return = Type;

    type Context<'a> = (&'a mut Ambient, Env);

    fn infer(&self, (ambient, mut env): Self::Context<'_>) -> Self::Return {
        env.set_location(self.span.clone());

        match &self.data {
            ExprKind::Lambda(lam) => {
                let mut bindings = HashMap::new();
                let ty = lam.param.infer((env.clone(), &mut bindings));

                for (k, (_, t)) in bindings {
                    env.add_variable(k, t)
                }

                let body = lam.body.infer((ambient, env.clone()));

                Type::arrow(ty, env.new_hole(Kind::effect()), body)
            }
            ExprKind::Variable(var) => env.variables.get(var).unwrap().clone(),
            ExprKind::Constructor(cons) => env.get_module_constructor(cons).0,
            ExprKind::Function(name) => env.get_module_let(name),
            ExprKind::Let(let_) => {
                let mut bindings = HashMap::new();
                let ty = let_.pattern.infer((env.clone(), &mut bindings));

                let_.body.check(ty, (ambient, env.clone()));

                for (k, (_, t)) in bindings {
                    env.add_variable(k, t)
                }

                let_.value.infer((ambient, env))
            }

            ExprKind::When(when) => {
                let scrutinee = when.scrutinee.infer((ambient, env.clone()));

                let (types, ret) =
                    (1, &when.arms.iter().collect::<Vec<_>>()).infer((ambient, env.clone()));

                if types.len() == 1 {
                    let ty = types.first().unwrap().clone();
                    Type::unify(env.clone(), scrutinee, ty);
                    ret
                } else {
                    Type::error()
                }
            }
            ExprKind::Do(not) => {
                let Some(unit_qual) = env.import("Unit") else {
                    return Type::error();
                };

                let unit = Type::variable(unit_qual);

                let mut res_ty = unit.clone();

                for expr in &not.statements {
                    env.set_location(expr.span.clone());
                    match &expr.data {
                        StatementKind::Let(let_) => {
                            let mut bindings = HashMap::new();
                            let ty = let_.pattern.infer((env.clone(), &mut bindings));

                            let_.expr.check(ty, (ambient, env.clone()));

                            for (k, (_, t)) in bindings {
                                env.add_variable(k, t)
                            }

                            res_ty = unit.clone()
                        }
                        StatementKind::Expr(e) => {
                            res_ty = e.infer((ambient, env.clone()));
                        }
                        StatementKind::Error => todo!(),
                    }
                }

                res_ty
            }
            ExprKind::Application(app) => {
                let mut ty = app.func.infer((ambient, env.clone()));

                for arg in &app.args {
                    let ty2 = arg.apply(ty, (ambient, env.clone()));
                    ty = ty2;
                }

                ty
            }

            ExprKind::Literal(l) => l.infer(&env),
            ExprKind::Annotation(ann) => {
                let (ty, _) = ann.ty.infer(&env);
                ann.expr.check(ty.clone(), (ambient, env.clone()));
                ty
            }

            ExprKind::Tuple(tuple) => {
                let mut types = Vec::new();

                for expr in &tuple.exprs {
                    types.push(expr.infer((ambient, env.clone())));
                }

                Type::tuple(types)
            }
            ExprKind::Projection(proj) => {
                let typ = proj.expr.infer((ambient, env.clone()));

                let Some((n, args)) = typ.destruct() else {
                    env.report(crate::error::TypeErrorKind::NotImplemented);
                    return Type::error()
                };

                let data = env.get_module_ty(&n);

                let Def::Record(fields) = data.def else {
                    env.report(crate::error::TypeErrorKind::NotImplemented);
                    return Type::error()
                };

                let Some(field) = fields.iter().find(|x| x.name == proj.field) else {
                    env.report(crate::error::TypeErrorKind::NotImplemented);
                    return Type::error()
                };

                let mut find = env.get_module_field(field);

                instantiate_with(&mut find, args);

                find
            }
            ExprKind::RecordInstance(inst) => {
                let rec = env.get_module_ty(&inst.name);

                let Def::Record(fields) = rec.def else {
                    env.report(crate::error::TypeErrorKind::NotARecord);
                    return Type::error()
                };

                let mut used = HashSet::new();

                let available = fields
                    .iter()
                    .map(|x| (x.name.clone(), env.get_module_field(x)))
                    .collect::<HashMap<Symbol, Type>>();

                let binders = (0..rec.binders)
                    .map(|_| env.new_hole(Kind::new_hole()))
                    .collect::<Vec<_>>();

                for field in &inst.fields {
                    env.set_location(field.1.span.clone());

                    let ty = field.1.infer((ambient, env.clone()));

                    let Some(mut typ) = available.get(&field.0).cloned() else {
                        env.report(crate::error::TypeErrorKind::NotFoundField);
                        return Type::error()
                    };

                    instantiate_with(&mut typ, binders.clone());

                    if !used.insert(field.0.clone()) {
                        env.report(crate::error::TypeErrorKind::DuplicatedField);
                        return Type::error();
                    }

                    Type::unify(env.clone(), ty, typ.clone());
                }

                let available = available.keys().cloned().collect::<HashSet<_>>();

                let diff = available.difference(&used).collect::<Vec<_>>();

                if !diff.is_empty() {
                    for available in diff {
                        env.set_location(self.span.clone());
                        env.report(crate::error::TypeErrorKind::MissingField(available.clone()));
                    }
                    return Type::error();
                }

                Type::app(Type::variable(inst.name.clone()), binders)
            }

            ExprKind::RecordUpdate(rec) => {
                let expr_ty = rec.expr.infer((ambient, env.clone()));

                let Some((n, binders)) = expr_ty.destruct() else {
                    env.report(crate::error::TypeErrorKind::NotImplemented);
                    return Type::error()
                };

                let record = env.get_module_ty(&n);

                let Def::Record(fields) = record.def else {
                    env.report(crate::error::TypeErrorKind::NotARecord);
                    return Type::error()
                };

                let mut used = HashSet::new();

                let available = fields
                    .iter()
                    .map(|x| (x.name.clone(), env.get_module_field(x)))
                    .collect::<HashMap<Symbol, Type>>();

                for field in &rec.fields {
                    env.set_location(field.1.span.clone());

                    let ty = field.1.infer((ambient, env.clone()));

                    let Some(mut typ) = available.get(&field.0).cloned() else {
                        env.report(crate::error::TypeErrorKind::NotFoundField);
                        return Type::error()
                    };

                    instantiate_with(&mut typ, binders.clone());

                    if !used.insert(field.0.clone()) {
                        env.report(crate::error::TypeErrorKind::DuplicatedField);
                        return Type::error();
                    }

                    Type::unify(env.clone(), ty, typ.clone());
                }

                expr_ty
            }

            ExprKind::Handler(_) => {
                env.report(crate::error::TypeErrorKind::NotImplemented);
                Type::error()
            }
            ExprKind::Cases(_) => {
                env.report(crate::error::TypeErrorKind::NotImplemented);
                Type::error()
            }
            ExprKind::Effect(app) => env.get_module_effect(app),

            ExprKind::Error => Type::error(),
        }
    }
}

fn instantiate_with(find: &mut Type, args: Vec<Type>) {
    let mut i = 0;
    while let TypeKind::Forall(name, _, to) = find.deref().as_ref() {
        *find = to.substitute(name.clone(), args[i].clone());
        i += 1;
    }
}
