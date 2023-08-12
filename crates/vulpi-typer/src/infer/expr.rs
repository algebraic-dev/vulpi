//! Inference of expressions

use crate::r#type::eval::Eval;
use crate::r#type::eval::Quote;
use crate::r#type::r#virtual;
use crate::r#type::TypeKind;

use crate::Kind;

use crate::check::Check;
use im_rc::HashMap;
use im_rc::HashSet;
use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract::Qualified;
use vulpi_syntax::{
    r#abstract::Statement,
    r#abstract::{Expr, ExprKind, StatementKind},
};

use crate::{
    errors::TypeErrorKind,
    r#type::{r#virtual::Virtual, Effect},
    Context, Env, Type,
};

use super::Infer;

impl Infer for Expr {
    type Return = Type<Virtual>;

    type Context<'a> = (&'a mut Context, &'a Effect<Virtual>, Env);

    fn infer(&self, (ctx, ambient, mut env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        match &self.data {
            ExprKind::Application(app) => {
                let mut ty = app.func.infer((ctx, ambient, env.clone()));
                for arg in &app.args {
                    env.on(arg.span.clone());

                    let arg_ty = arg.infer((ctx, ambient, env.clone()));

                    if let Some((left, effs, right)) = ctx.as_function(&env, ty.deref()) {
                        let opened = ctx.open(&env, effs);
                        ctx.subsumes(env.clone(), left, arg_ty);
                        ctx.subsumes(env.clone(), ambient.clone(), opened);

                        ty = right;
                    } else {
                        ctx.report(
                            &env,
                            TypeErrorKind::NotAFunction(env.clone(), ty.quote(env.level)),
                        );
                        return Type::error();
                    }
                }

                ty
            }
            ExprKind::Variable(m) => env.vars.get(m).unwrap().clone(),
            ExprKind::Constructor(n) => ctx.modules.constructor(n).0,
            ExprKind::Function(n) => ctx.modules.let_decl(n).typ,
            ExprKind::Effect(n) => ctx.modules.effect(n).0,
            ExprKind::Let(e) => {
                let val_ty = e.body.infer((ctx, ambient, env.clone()));

                let mut hashmap = Default::default();
                let pat_ty = e.pattern.infer((ctx, &mut hashmap, env.clone()));

                ctx.subsumes(env.clone(), pat_ty, val_ty);

                for binding in hashmap {
                    env.add_var(binding.0, binding.1)
                }

                e.value.infer((ctx, ambient, env.clone()))
            }
            ExprKind::Tuple(t) => {
                let mut types = Vec::new();

                for ty in &t.exprs {
                    let ty = ty.infer((ctx, ambient, env.clone()));
                    types.push(ty);
                }

                Type::tuple(types)
            }
            ExprKind::Error => Type::error(),
            ExprKind::When(when) => {
                // TODO: Check mode
                let ret_type = ctx.hole(&env, Kind::typ());
                let (_, arms, ret) = when.arms.infer((ctx, ambient.clone(), env.clone()));

                let scrutinee = when
                    .scrutinee
                    .iter()
                    .map(|x| x.infer((ctx, ambient, env.clone())))
                    .collect::<Vec<_>>();

                if arms.len() != scrutinee.len() {
                    ctx.report(&env, TypeErrorKind::WrongArity(arms.len(), scrutinee.len()));
                }

                for (arm, scrutinee) in arms.into_iter().zip(scrutinee.into_iter()) {
                    ctx.subsumes(env.clone(), arm, scrutinee);
                }

                ctx.subsumes(env, ret_type.clone(), ret);

                ret_type
            }
            ExprKind::Do(block) => {
                let mut ty = Type::tuple(vec![]);

                for stmt in &block.statements {
                    (ty, env) = stmt.infer((ctx, ambient, env.clone()));
                }

                ty
            }
            ExprKind::Literal(n) => n.infer((ctx, env)),
            ExprKind::Annotation(ann) => {
                let ty = ann.expr.infer((ctx, ambient, env.clone()));
                let (typ, _) = ann.ty.infer((ctx, env.clone()));
                let right = typ.eval(&env);
                ctx.subsumes(env.clone(), ty, right.clone());
                right
            }
            ExprKind::Lambda(lam) => {
                let mut hashmap = Default::default();
                let pat_ty = lam.param.infer((ctx, &mut hashmap, env.clone()));

                for binding in hashmap {
                    env.add_var(binding.0, binding.1)
                }

                let ambient = ctx.lacks(&env, Default::default());
                let body = lam.body.infer((ctx, &ambient, env.clone()));

                Type::new(TypeKind::Arrow(r#virtual::Pi {
                    ty: pat_ty,
                    effs: ambient,
                    body,
                }))
            }
            ExprKind::Projection(expr) => {
                let ty = expr.expr.infer((ctx, ambient, env.clone()));

                let (head, spine) = ty.application_spine();

                let TypeKind::Variable(name) = head.as_ref()  else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return Type::error();
                };

                let typ = ctx.modules.typ(name);

                let crate::module::Def::Record(rec) = typ.def else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return Type::error();
                };

                let Some(field) = rec.iter().find(|x| x.name == expr.field) else {
                    ctx.report(&env, TypeErrorKind::NotFoundField);
                    return Type::error();
                };

                let field = ctx.modules.field(field);

                let eval_ty = field.eval(&env);

                ctx.instantiate_with_args(&eval_ty, spine)
            }
            ExprKind::RecordInstance(instance) => {
                let typ = ctx.modules.typ(&instance.name);

                let crate::module::Def::Record(rec) = typ.def else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return Type::error();
                };

                let iter = rec.into_iter().map(|x| (x.name.clone(), x));

                let available: HashMap<Symbol, Qualified> = HashMap::from_iter(iter);
                let mut used = HashSet::<Symbol>::default();

                let binders = typ
                    .binders
                    .iter()
                    .map(|x| ctx.hole::<Virtual>(&env, x.1.clone()))
                    .collect::<Vec<_>>();

                let ret_type = Type::<Virtual>::application(
                    Type::variable(instance.name.clone()),
                    binders.clone(),
                );

                for (span, name, expr) in &instance.fields {
                    env.on(span.clone());

                    let Some(qualified) = available.get(name) else {
                        ctx.report(&env, TypeErrorKind::NotFoundField);
                        continue
                    };

                    if used.contains(name) {
                        ctx.report(&env, TypeErrorKind::DuplicatedField);
                        continue;
                    }

                    let field = ctx.modules.field(qualified).eval(&env);
                    let inst_field = ctx.instantiate_with_args(&field, binders.clone());

                    expr.check(inst_field.clone(), (ctx, ambient, env.clone()));

                    used.insert(name.clone());
                }

                let diff = available
                    .keys()
                    .cloned()
                    .collect::<HashSet<_>>()
                    .difference(used);

                for key in diff {
                    ctx.report(&env, TypeErrorKind::MissingField(key));
                }

                ret_type
            }
            ExprKind::RecordUpdate(update) => {
                let typ = update.expr.infer((ctx, ambient, env.clone()));

                let (head, binders) = typ.application_spine();

                let TypeKind::Variable(name) = head.as_ref() else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return Type::error();
                };

                let Some(typ) = ctx.modules.get(&name.path).types.get(&name.name).cloned() else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return Type::error();
                };

                let crate::module::Def::Record(rec) = &typ.def else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return Type::error();
                };

                let iter = rec.iter().map(|x| (x.name.clone(), x.clone()));

                let available: HashMap<Symbol, Qualified> = HashMap::from_iter(iter);
                let mut used = HashSet::<Symbol>::default();

                let ret_type =
                    Type::<Virtual>::application(Type::variable(name.clone()), binders.clone());

                for (span, name, expr) in &update.fields {
                    env.on(span.clone());

                    let Some(qualified) = available.get(name) else {
                        ctx.report(&env, TypeErrorKind::NotFoundField);
                        continue
                    };

                    if used.contains(name) {
                        ctx.report(&env, TypeErrorKind::DuplicatedField);
                        continue;
                    }

                    let field = ctx.modules.field(qualified).eval(&env);
                    let inst_field = ctx.instantiate_with_args(&field, binders.clone());

                    expr.check(inst_field.clone(), (ctx, ambient, env.clone()));

                    used.insert(name.clone());
                }

                ret_type
            }
            ExprKind::Handler(_) => {
                ctx.report(&env, TypeErrorKind::NotImplemented);
                Type::error()
            }
            ExprKind::Cases(_) => {
                ctx.report(&env, TypeErrorKind::NotImplemented);
                Type::error()
            }
        }
    }
}

impl Infer for Statement {
    type Return = (Type<Virtual>, Env);

    type Context<'a> = (&'a mut Context, &'a Effect<Virtual>, Env);

    fn infer(&self, (ctx, ambient, mut env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());
        match &self.data {
            StatementKind::Let(decl) => {
                let val_ty = decl.expr.infer((ctx, ambient, env.clone()));

                let mut hashmap = Default::default();
                let pat_ty = decl.pattern.infer((ctx, &mut hashmap, env.clone()));

                ctx.subsumes(env.clone(), pat_ty, val_ty);

                for binding in hashmap {
                    env.add_var(binding.0, binding.1)
                }

                (Type::tuple(vec![]), env)
            }
            StatementKind::Expr(expr) => {
                let ty = expr.infer((ctx, ambient, env.clone()));
                (ty, env)
            }
            StatementKind::Error => (Type::error(), env),
        }
    }
}
