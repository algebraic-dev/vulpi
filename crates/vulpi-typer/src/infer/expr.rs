//! Inference of expressions

use crate::Real;
use crate::r#type::eval::Eval;
use crate::r#type::eval::Quote;
use crate::r#type::r#virtual;
use crate::r#type::TypeKind;

use crate::Kind;

use crate::check::Check;
use im_rc::HashMap;
use im_rc::HashSet;
use vulpi_intern::Symbol;
use vulpi_syntax::elaborated;
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
    type Return = (Type<Virtual>, elaborated::Expr<Type<Real>>);

    type Context<'a> = (&'a mut Context, &'a Effect<Virtual>, Env);

    fn infer(&self, (ctx, ambient, mut env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        match &self.data {
            ExprKind::Application(app) => {
                let (mut ty, func_elab) = app.func.infer((ctx, ambient, env.clone()));

                let mut elab_args = Vec::new();

                for arg in &app.args {
                    env.on(arg.span.clone());

                    let (arg_ty, arg_elab) = arg.infer((ctx, ambient, env.clone()));
                    elab_args.push(arg_elab);

                    if let Some((left, effs, right)) = ctx.as_function(&env, ty.deref()) {
                        let opened = ctx.open(&env, effs);
                        ctx.subsumes(env.clone(), left, arg_ty);


                        env.on(self.span.clone());
                        ctx.subsumes(env.clone(), ambient.clone(), opened);

                        ty = right;
                    } else {
                        ctx.report(
                            &env,
                            TypeErrorKind::NotAFunction(env.clone(), ty.quote(env.level)),
                        );
                        return (Type::error(), Box::new(elaborated::ExprKind::Error));
                    }
                }

                (
                    ty.clone(),
                    Box::new(elaborated::ExprKind::Application(
                        elaborated::ApplicationExpr {
                            typ: ty.quote(env.level),
                            func: func_elab,
                            args: elab_args,
                        },
                    )),
                )
            }
            ExprKind::Variable(m) => (
                env.vars.get(m).unwrap().clone(),
                Box::new(elaborated::ExprKind::Variable(m.clone())),
            ),
            ExprKind::Constructor(n) => (
                ctx.modules.constructor(n).0.eval(&env),
                Box::new(elaborated::ExprKind::Constructor(n.clone())),
            ),
            ExprKind::Function(n) => (
                ctx.modules.let_decl(n).typ.clone(),
                Box::new(elaborated::ExprKind::Function(n.clone())),
            ),
            ExprKind::Effect(n) => (
                ctx.modules.effect(n).0,
                Box::new(elaborated::ExprKind::Effect(n.clone())),
            ),
            ExprKind::Let(e) => {
                let (val_ty, body_elab) = e.body.infer((ctx, ambient, env.clone()));

                let mut hashmap = Default::default();
                let (pat_ty, pat_elab) = e.pattern.infer((ctx, &mut hashmap, env.clone()));

                ctx.subsumes(env.clone(), pat_ty, val_ty);

                for binding in hashmap {
                    env.add_var(binding.0, binding.1)
                }

                let (ty, value_elab) = e.value.infer((ctx, ambient, env.clone()));

                (
                    ty,
                    Box::new(elaborated::ExprKind::Let(elaborated::LetExpr {
                        pattern: pat_elab,
                        value: value_elab,
                        body: body_elab,
                    })),
                )
            }
            ExprKind::Tuple(t) => {
                let mut types = Vec::new();
                let mut elaborated = Vec::new();

                for ty in &t.exprs {
                    let (ty, elab) = ty.infer((ctx, ambient, env.clone()));
                    types.push(ty);
                    elaborated.push(elab);
                }

                (
                    Type::tuple(types),
                    Box::new(elaborated::ExprKind::Tuple(
                        vulpi_syntax::elaborated::Tuple { exprs: elaborated },
                    )),
                )
            }
            ExprKind::Error => (Type::error(), Box::new(elaborated::ExprKind::Error)),
            ExprKind::When(when) => {
                // TODO: Check mode
                let ret_type = ctx.hole(&env, Kind::typ());
                let (_, arms, ret, elab_arms) =
                    when.arms.infer((ctx, ambient.clone(), env.clone()));

                if arms.len() != when.scrutinee.len() {
                    ctx.report(
                        &env,
                        TypeErrorKind::WrongArity(arms.len(), when.scrutinee.len()),
                    );
                }

                let mut elab_scrutinee = Vec::new();

                for (arm, scrutinee) in arms.into_iter().zip(when.scrutinee.iter()) {
                    let elab = scrutinee.check(arm, (ctx, ambient, env.clone()));
                    elab_scrutinee.push(elab);
                }

                ctx.subsumes(env, ret_type.clone(), ret);

                (
                    ret_type,
                    Box::new(elaborated::ExprKind::When(elaborated::WhenExpr {
                        scrutinee: elab_scrutinee,
                        arms: elab_arms,
                    })),
                )
            }
            ExprKind::Do(block) => {
                let mut ty = Type::tuple(vec![]);
                let mut stmts = Vec::new();

                for stmt in &block.statements {
                    let (new_ty, new_env, stmt) = stmt.infer((ctx, ambient, env.clone()));
                    ty = new_ty;
                    env = new_env;

                    stmts.push(stmt);
                }

                (ty, Box::new(elaborated::ExprKind::Do(stmts)))
            }
            ExprKind::Literal(n) => {
                let (ty, elab) = n.infer((ctx, env));
                (ty, Box::new(elaborated::ExprKind::Literal(elab)))
            }
            ExprKind::Annotation(ann) => {
                let (ty, elab_expr) = ann.expr.infer((ctx, ambient, env.clone()));
                let (typ, _) = ann.ty.infer((ctx, env.clone()));
                let right = typ.eval(&env);
                ctx.subsumes(env.clone(), ty, right.clone());
                (right, elab_expr)
            }
            ExprKind::Lambda(lam) => {
                let mut hashmap = Default::default();
                let (pat_ty, elab_pat) = lam.param.infer((ctx, &mut hashmap, env.clone()));

                for binding in hashmap {
                    env.add_var(binding.0, binding.1)
                }

                let ambient = ctx.lacks(&env, Default::default());
                let (body, elab_body) = lam.body.infer((ctx, &ambient, env.clone()));

                (
                    Type::new(TypeKind::Arrow(r#virtual::Pi {
                        ty: pat_ty,
                        effs: ambient,
                        body,
                    })),
                    Box::new(elaborated::ExprKind::Lambda(elaborated::LambdaExpr {
                        param: elab_pat,
                        body: elab_body,
                    })),
                )
            }
            ExprKind::Projection(expr) => {
                let (ty, elab_expr) = expr.expr.infer((ctx, ambient, env.clone()));

                let (head, spine) = ty.application_spine();

                let TypeKind::Variable(name) = head.as_ref()  else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Box::new(elaborated::ExprKind::Error));
                };

                let typ = ctx.modules.typ(name);

                let crate::module::Def::Record(rec) = typ.def else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Box::new(elaborated::ExprKind::Error));
                };

                let Some(field_name) = rec.iter().find(|x| x.name == expr.field) else {
                    ctx.report(&env, TypeErrorKind::NotFoundField);
                    return (Type::error(), Box::new(elaborated::ExprKind::Error));
                };

                let field = ctx.modules.field(field_name);

                let eval_ty = field.eval(&env);

                (
                    ctx.instantiate_with_args(&eval_ty, spine),
                    Box::new(elaborated::ExprKind::Projection(
                        elaborated::ProjectionExpr {
                            expr: elab_expr,
                            field: field_name.clone(),
                        },
                    )),
                )
            }
            ExprKind::RecordInstance(instance) => {
                let typ = ctx.modules.typ(&instance.name);

                let crate::module::Def::Record(rec) = typ.def else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Box::new(elaborated::ExprKind::Error));
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

                let mut elab_fields = Vec::new();

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

                    let elab_expr = expr.check(inst_field.clone(), (ctx, ambient, env.clone()));

                    elab_fields.push((name.clone(), elab_expr));

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

                (
                    ret_type,
                    Box::new(elaborated::ExprKind::RecordInstance(
                        elaborated::RecordInstance {
                            name: instance.name.clone(),
                            fields: elab_fields,
                        },
                    )),
                )
            }
            ExprKind::RecordUpdate(update) => {
                let (typ, elab_expr) = update.expr.infer((ctx, ambient, env.clone()));

                let (head, binders) = typ.application_spine();

                let TypeKind::Variable(name) = head.as_ref() else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Box::new(elaborated::ExprKind::Error));
                };

                let Some(typ) = ctx.modules.get(&name.path).types.get(&name.name).cloned() else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Box::new(elaborated::ExprKind::Error));
                };

                let crate::module::Def::Record(rec) = &typ.def else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Box::new(elaborated::ExprKind::Error));
                };

                let iter = rec.iter().map(|x| (x.name.clone(), x.clone()));

                let available: HashMap<Symbol, Qualified> = HashMap::from_iter(iter);
                let mut used = HashSet::<Symbol>::default();

                let ret_type =
                    Type::<Virtual>::application(Type::variable(name.clone()), binders.clone());

                let mut elab_fields = Vec::new();

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

                    let elab = expr.check(inst_field.clone(), (ctx, ambient, env.clone()));

                    elab_fields.push((name.clone(), elab));

                    used.insert(name.clone());
                }

                (
                    ret_type,
                    Box::new(elaborated::ExprKind::RecordUpdate(
                        elaborated::RecordUpdate {
                            expr: elab_expr,
                            fields: elab_fields,
                        },
                    )),
                )
            }
            ExprKind::Handler(h) => {

                let scrutinee = ctx.hole::<Virtual>(&env, Type::typ());

                let (handle_type, elab_handler) = h.with.infer((ctx, ambient, env.clone()));

                let Some((left, eff, right)) = ctx.as_function(&env, handle_type.clone()) else {
                    ctx.report(&env, TypeErrorKind::NotAFunction(env.clone(), handle_type.quote(env.level)));
                    return (Type::error(), Box::new(elaborated::ExprKind::Error));  
                };

                let request = ctx.find_prelude_type("Request", env.clone());

                let removed_effect = ctx.hole::<Virtual>(&env, Type::effect());

                let app = Type::<Virtual>::application(
                    request,
                    vec![removed_effect.clone(), scrutinee.clone()],
                );

                let (var, _) = removed_effect.application_spine();

                ctx.subsumes(env.clone(), left, app);
  
                let new_ambient = if let TypeKind::Variable(name) = var.deref().as_ref() {
                    Type::<Virtual>::extend(name.clone(), removed_effect, ambient.clone())
                } else {
                    ambient.clone()
                };
                 

                let elab_expr = h.expr.check(scrutinee, (ctx, &new_ambient, env.clone()));

                let eff = ctx.open(&env, eff);
                ctx.subsumes(env.clone(), ambient.clone(), eff);

                (right, Box::new(elaborated::ExprKind::Handler(elaborated::HandlerExpr { 
                    expr: elab_expr, 
                    with: elab_handler 
                })))
            }
            ExprKind::Cases(_) => {
                let ty = ctx.hole::<Virtual>(&env, Type::typ());
                (ty.clone(), self.check(ty, (ctx, ambient, env.clone())))
            }
        }
    }
}

impl Infer for Statement {
    type Return = (Type<Virtual>, Env, elaborated::Statement<Type<Real>>);

    type Context<'a> = (&'a mut Context, &'a Effect<Virtual>, Env);

    fn infer(&self, (ctx, ambient, mut env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());
        match &self.data {
            StatementKind::Let(decl) => {
                let mut hashmap = Default::default();
                let (pat_ty, elab_pat) = decl.pattern.infer((ctx, &mut hashmap, env.clone()));

                let elab_expr = decl.expr.check(pat_ty, (ctx, ambient, env.clone()));

                for binding in hashmap {
                    env.add_var(binding.0, binding.1)
                }

                (
                    Type::tuple(vec![]),
                    env,
                    elaborated::StatementKind::Let(elaborated::LetStatement {
                        pattern: elab_pat,
                        expr: elab_expr,
                    }),
                )
            }
            StatementKind::Expr(expr) => {
                let (ty, elab_expr) = expr.infer((ctx, ambient, env.clone()));
                (ty, env, elaborated::StatementKind::Expr(elab_expr))
            }
            StatementKind::Error => (Type::error(), env, elaborated::StatementKind::Error),
        }
    }
}
