use std::collections::HashSet;

use crate::apply::Apply;
use crate::check::Check;
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

    type Context<'a> = Env;

    fn infer(&self, mut context: Self::Context<'_>) -> Self::Return {
        let mut tys = Vec::new();

        let mut bindings = HashMap::new();

        for pat in &self.patterns {
            tys.push(pat.infer((context.clone(), &mut bindings)))
        }

        for binding in bindings {
            context.add_variable(binding.0, binding.1 .1)
        }

        if let Some(guard) = &self.guard {
            let right = context.import("Bool").unwrap();
            let right = Type::variable(right);

            guard.check(right, context.clone());
        }

        let result = self.expr.infer(context);

        (tys, result)
    }
}

impl Infer for (usize, &Vec<&PatternArm>) {
    type Return = (Vec<Type>, Type);

    type Context<'a> = Env;

    fn infer(&self, context: Self::Context<'_>) -> Self::Return {
        let (size, arms) = self;
        let types = (0..*size).map(|_| context.new_hole()).collect::<Vec<_>>();
        let ret = context.new_hole();

        for arm in *arms {
            if arm.patterns.len() != *size {
                context.report(crate::error::TypeErrorKind::WrongArity(
                    *size,
                    arm.patterns.len(),
                ));
                return (Vec::new(), Type::error());
            }

            let tys = arm.check(ret.clone(), context.clone());

            for (left, right) in types.iter().zip(tys.into_iter()) {
                left.sub(context.clone(), right);
            }
        }

        (types, ret)
    }
}

impl Infer for Expr {
    type Return = Type;

    type Context<'a> = Env;

    fn infer(&self, mut context: Self::Context<'_>) -> Self::Return {
        context.set_location(self.span.clone());

        match &self.data {
            ExprKind::Lambda(lam) => {
                let mut bindings = HashMap::new();
                let ty = lam.param.infer((context.clone(), &mut bindings));

                for (k, (_, t)) in bindings {
                    context.add_variable(k, t)
                }

                let body = lam.body.infer(context);

                Type::arrow(ty, body)
            }
            ExprKind::Variable(var) => context.variables.get(var).unwrap().clone(),
            ExprKind::Constructor(cons) => context.get_module_constructor(cons).0,
            ExprKind::Function(name) => context.get_module_let(name),
            ExprKind::Let(let_) => {
                let mut bindings = HashMap::new();
                let ty = let_.pattern.infer((context.clone(), &mut bindings));

                let_.body.check(ty, context.clone());

                for (k, (_, t)) in bindings {
                    context.add_variable(k, t)
                }

                let_.value.infer(context)
            }

            ExprKind::When(when) => {
                let scrutinee = when.scrutinee.infer(context.clone());

                let (types, ret) =
                    (1, &when.arms.iter().collect::<Vec<_>>()).infer(context.clone());

                if types.len() == 1 {
                    let ty = types.first().unwrap().clone();
                    Type::unify(context.clone(), scrutinee, ty);
                    ret
                } else {
                    Type::error()
                }
            }
            ExprKind::Do(not) => {
                let Some(unit_qual) = context.import("Unit") else {
                    return Type::error();
                };

                let unit = Type::variable(unit_qual);

                let mut res_ty = unit.clone();

                for expr in &not.statements {
                    context.set_location(expr.span.clone());
                    match &expr.data {
                        StatementKind::Let(let_) => {
                            let mut bindings = HashMap::new();
                            let ty = let_.pattern.infer((context.clone(), &mut bindings));

                            let_.expr.check(ty, context.clone());

                            for (k, (_, t)) in bindings {
                                context.add_variable(k, t)
                            }

                            res_ty = unit.clone()
                        }
                        StatementKind::Expr(e) => {
                            res_ty = e.infer(context.clone());
                        }
                        StatementKind::Error => todo!(),
                    }
                }

                res_ty
            }
            ExprKind::Application(app) => {
                let mut ty = app.func.infer(context.clone());

                for arg in &app.args {
                    let ty2 = arg.apply(ty, context.clone());
                    ty = ty2;
                }

                ty
            }

            ExprKind::Literal(l) => l.infer(&context),
            ExprKind::Annotation(ann) => {
                let (ty, _) = ann.ty.infer(&context);
                ann.expr.check(ty.clone(), context.clone());
                ty
            }

            ExprKind::Tuple(tuple) => {
                let mut types = Vec::new();

                for expr in &tuple.exprs {
                    types.push(expr.infer(context.clone()));
                }

                Type::tuple(types)
            }
            ExprKind::Projection(proj) => {
                let typ = proj.expr.infer(context.clone());

                let Some((n, args)) = typ.destruct() else {
                    context.report(crate::error::TypeErrorKind::NotImplemented);
                    return Type::error()
                };

                let data = context.get_module_ty(&n);

                let Def::Record(fields) = data.def else {
                    context.report(crate::error::TypeErrorKind::NotImplemented);
                    return Type::error()
                };

                let Some(field) = fields.iter().find(|x| x.name == proj.field) else {
                    context.report(crate::error::TypeErrorKind::NotImplemented);
                    return Type::error()
                };

                let mut find = context.get_module_field(field);

                instantiate_with(&mut find, args);

                find
            }
            ExprKind::RecordInstance(inst) => {
                let rec = context.get_module_ty(&inst.name);

                let Def::Record(fields) = rec.def else {
                    context.report(crate::error::TypeErrorKind::NotARecord);
                    return Type::error()
                };

                let mut used = HashSet::new();

                let available = fields
                    .iter()
                    .map(|x| (x.name.clone(), context.get_module_field(x)))
                    .collect::<HashMap<Symbol, Type>>();

                let binders = (0..rec.binders)
                    .map(|_| context.new_hole())
                    .collect::<Vec<_>>();

                for field in &inst.fields {
                    context.set_location(field.1.span.clone());

                    let ty = field.1.infer(context.clone());

                    let Some(mut typ) = available.get(&field.0).cloned() else {
                        context.report(crate::error::TypeErrorKind::NotFoundField);
                        return Type::error()
                    };

                    instantiate_with(&mut typ, binders.clone());

                    if !used.insert(field.0.clone()) {
                        context.report(crate::error::TypeErrorKind::DuplicatedField);
                        return Type::error();
                    }

                    Type::unify(context.clone(), ty, typ.clone());
                }

                let available = available.keys().cloned().collect::<HashSet<_>>();

                let diff = available.difference(&used).collect::<Vec<_>>();

                if !diff.is_empty() {
                    for available in diff {
                        context.set_location(self.span.clone());
                        context
                            .report(crate::error::TypeErrorKind::MissingField(available.clone()));
                    }
                    return Type::error();
                }

                Type::app(Type::variable(inst.name.clone()), binders)
            }

            ExprKind::RecordUpdate(rec) => {
                let expr_ty = rec.expr.infer(context.clone());

                let Some((n, binders)) = expr_ty.destruct() else {
                    context.report(crate::error::TypeErrorKind::NotImplemented);
                    return Type::error()
                };

                let record = context.get_module_ty(&n);

                let Def::Record(fields) = record.def else {
                    context.report(crate::error::TypeErrorKind::NotARecord);
                    return Type::error()
                };

                let mut used = HashSet::new();

                let available = fields
                    .iter()
                    .map(|x| (x.name.clone(), context.get_module_field(x)))
                    .collect::<HashMap<Symbol, Type>>();

                for field in &rec.fields {
                    context.set_location(field.1.span.clone());

                    let ty = field.1.infer(context.clone());

                    let Some(mut typ) = available.get(&field.0).cloned() else {
                        context.report(crate::error::TypeErrorKind::NotFoundField);
                        return Type::error()
                    };

                    instantiate_with(&mut typ, binders.clone());

                    if !used.insert(field.0.clone()) {
                        context.report(crate::error::TypeErrorKind::DuplicatedField);
                        return Type::error();
                    }

                    Type::unify(context.clone(), ty, typ.clone());
                }

                expr_ty
            }

            ExprKind::Handler(_) => {
                context.report(crate::error::TypeErrorKind::NotImplemented);
                Type::error()
            }
            ExprKind::Cases(_) => {
                context.report(crate::error::TypeErrorKind::NotImplemented);
                Type::error()
            }
            ExprKind::Effect(_) => {
                context.report(crate::error::TypeErrorKind::NotImplemented);
                Type::error()
            }

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
