//! Inference of expressions

use crate::coverage::Problem;
use crate::coverage::Witness;
use crate::r#type::eval::Eval;
use crate::r#type::eval::Quote;
use crate::r#type::r#virtual;
use crate::r#type::TypeKind;
use crate::Real;

use crate::Kind;

use crate::check::Check;
use im_rc::HashMap;
use im_rc::HashSet;
use vulpi_intern::Symbol;
use vulpi_location::Spanned;
use vulpi_syntax::elaborated;
use vulpi_syntax::r#abstract::Qualified;
use vulpi_syntax::{
    r#abstract::Sttm,
    r#abstract::{Expr, ExprKind, SttmKind},
};

use crate::{
    errors::TypeErrorKind,
    r#type::r#virtual::Virtual,
    Context, Env, Type,
};

use super::Infer;

impl Infer for Expr {
    type Return = (Type<Virtual>, elaborated::Expr<Type<Real>>);

    type Context<'a> = (&'a mut Context, Env);

    fn infer(&self, (ctx, mut env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        let elem = match &self.data {
            ExprKind::Application(app) => {
                let (mut ty, func_elab) = app.func.infer((ctx, env.clone()));
                let mut elab_args = Vec::new();

                for arg in &app.args {
                    env.on(arg.span.clone());

                    let (arg_ty, arg_elab) = arg.infer((ctx, env.clone()));
                    elab_args.push(arg_elab);

                    if let Some((left, right)) = ctx.as_function(&env, ty.deref()) {
                        ctx.subsumes(env.clone(), arg_ty, left);
                        ty = right;
                    } else {
                        ctx.report(
                            &env,
                            TypeErrorKind::NotAFunction(env.clone(), ty.quote(env.level)),
                        );
                        return (Type::error(), Spanned::new(Box::new(elaborated::ExprKind::Error), self.span.clone()));
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
                Box::new(elaborated::ExprKind::Constructor(
                    ctx.modules.constructor(n).2,
                    n.clone(),
                )),
            ),
            ExprKind::Function(n) => {
                (
                ctx.modules.let_decl(n).typ.clone(),
                Box::new(elaborated::ExprKind::Function(
                    n.clone(),
                    ctx.modules.let_decl(n).typ.clone().quote(env.level),
                )),
                )
            }
            ExprKind::Let(e) => {
                let (val_ty, body_elab) = e.body.infer((ctx, env.clone()));

                let mut hashmap = Default::default();
                let (pat_ty, pat_elab) = e.pattern.infer((ctx, &mut hashmap, env.clone()));

                ctx.subsumes(env.clone(), pat_ty, val_ty);

                for binding in hashmap {
                    env.add_var(binding.0, binding.1)
                }

                let (ty, value_elab) = e.value.infer((ctx, env.clone()));

                (
                    ty,
                    Box::new(elaborated::ExprKind::Let(elaborated::LetExpr {
                        pattern: pat_elab,
                        next: value_elab,
                        body: body_elab,
                    })),
                )
            }
            ExprKind::Tuple(t) => {
                let mut types = Vec::new();
                let mut elaborated = Vec::new();

                for ty in &t.exprs {
                    let (ty, elab) = ty.infer((ctx, env.clone()));
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

                ctx.errored = false;

                let (_, arms, ret, elab_arms) =
                    when.arms.infer((ctx, env.clone()));

                let perform = !ctx.errored;

                if arms.len() != when.scrutinee.len() {
                    ctx.report(
                        &env,
                        TypeErrorKind::WrongArity(arms.len(), when.scrutinee.len()),
                    );
                }

                let mut elab_scrutinee = Vec::new();

                for (arm, scrutinee) in arms.iter().cloned().zip(when.scrutinee.iter()) {
                    let (ty, elab) = scrutinee.infer((ctx, env.clone()));
                    ctx.subsumes(env.clone(), arm, ty);
                    elab_scrutinee.push(elab);
                }

                ctx.subsumes(env.clone(), ret_type.clone(), ret);

                if perform {

                    let arms = arms.iter().map(|x| ctx.instantiate(&env, x)).collect();
                    
                    let problem = Problem::exhaustiveness(&elab_arms, arms);

                    if let Witness::NonExhaustive(case) = problem.exaustive(ctx, env.clone()) {
                        ctx.report(&env, TypeErrorKind::NonExhaustive(case));
                    };
                }

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

                for stmt in &block.sttms {
                    let (new_ty, new_env, stmt) = stmt.infer((ctx, &mut env.clone()));
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
                let (ty, elab_expr) = ann.expr.infer((ctx, env.clone()));
                let (typ, _) = ann.ty.infer((ctx, env.clone()));
                let right = typ.eval(&env);
                ctx.subsumes(env.clone(), ty, right.clone());
                (right, elab_expr.data)
            }
            ExprKind::Lambda(lam) => {
                let mut hashmap = Default::default();
                let (pat_ty, elab_pat) = lam.param.infer((ctx, &mut hashmap, env.clone()));

                for binding in hashmap {
                    env.add_var(binding.0, binding.1)
                }

                let (body, elab_body) = lam.body.infer((ctx, env.clone()));

                (
                    Type::new(TypeKind::Arrow(r#virtual::Pi {
                        ty: pat_ty,
                        body,
                    })),
                    Box::new(elaborated::ExprKind::Lambda(elaborated::LambdaExpr {
                        param: elab_pat,
                        body: elab_body,
                    })),
                )
            }
            ExprKind::Projection(expr) => {
                let (ty, elab_expr) = expr.expr.infer((ctx, env.clone()));

                let (head, spine) = ty.application_spine();

                let TypeKind::Variable(name) = head.as_ref()  else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Spanned::new(Box::new(elaborated::ExprKind::Error), self.span.clone()));
                };

                let typ = ctx.modules.typ(name);

                let crate::module::Def::Record(rec) = typ.def else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Spanned::new(Box::new(elaborated::ExprKind::Error), self.span.clone()));
                };

                let Some(field_name) = rec.iter().find(|x| x.name == expr.field) else {
                    ctx.report(&env, TypeErrorKind::NotFoundField);
                    return (Type::error(), Spanned::new(Box::new(elaborated::ExprKind::Error), self.span.clone()));
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
                    return (Type::error(), Spanned::new(Box::new(elaborated::ExprKind::Error), self.span.clone()));
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

                    let elab_expr = expr.check(inst_field.clone(), (ctx, env.clone()));

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
                let (typ, elab_expr) = update.expr.infer((ctx, env.clone()));

                let (head, binders) = typ.deref().application_spine();

                let TypeKind::Variable(name) = head.as_ref() else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Spanned::new(Box::new(elaborated::ExprKind::Error), self.span.clone()));
                };

                let Some(typ) = ctx.modules.get(&name.path).types.get(&name.name).cloned() else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Spanned::new(Box::new(elaborated::ExprKind::Error), self.span.clone()));
                };

                let crate::module::Def::Record(rec) = &typ.def else {
                    ctx.report(&env, TypeErrorKind::NotARecord);
                    return (Type::error(), Spanned::new(Box::new(elaborated::ExprKind::Error), self.span.clone()));
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

                    let elab = expr.check(inst_field.clone(), (ctx, env.clone()));

                    elab_fields.push((name.clone(), elab));

                    used.insert(name.clone());
                }

                (
                    ret_type,
                    Box::new(elaborated::ExprKind::RecordUpdate(
                        elaborated::RecordUpdate {
                            qualified: name.clone(),
                            expr: elab_expr,
                            fields: elab_fields,
                        },
                    )),
                )
            }
        };

        (elem.0, Spanned::new(elem.1, self.span.clone()))
    }
}

impl Infer for Sttm {
    type Return = (Type<Virtual>, Env, elaborated::Statement<Type<Real>>);

    type Context<'a> = (&'a mut Context, &'a mut Env);

    fn infer(&self, (ctx, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());
        match &self.data {
            SttmKind::Let(decl) => {
                let mut hashmap = Default::default();
                let (pat_ty, elab_pat) = decl.pat.infer((ctx, &mut hashmap, env.clone()));

                let elab_expr = decl.expr.check(pat_ty, (ctx, env.clone()));

                for binding in hashmap {
                    env.add_var(binding.0, binding.1)
                }

                (
                    Type::tuple(vec![]),
                    env.clone(),
                    elaborated::SttmKind::Let(elaborated::LetStatement {
                        pattern: elab_pat,
                        expr: elab_expr,
                    }),
                )
            }
            SttmKind::Expr(expr) => {
                let (ty, elab_expr) = expr.infer((ctx, env.clone()));
                (ty, env.clone(), elaborated::SttmKind::Expr(elab_expr))
            }
            SttmKind::Error => (Type::error(), env.clone(), elaborated::SttmKind::Error),
        }
    }
}
