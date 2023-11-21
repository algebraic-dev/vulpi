//! This module compiles a TypedTree to a Vulpi IR tree called Lambda. This is the first step in
//! lowering the AST to a form that is easier to work with for code generation.

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_location::Spanned;
use vulpi_syntax::{
    elaborated,
    lambda::{self, ExprKind},
    r#abstract::Qualified,
};
use vulpi_typer::{Real, Type};

use crate::pattern::{self, pattern_binders};

#[derive(Default)]
pub struct Ctx {
    var: usize,
}

pub fn compile_match_with_names(
    scrutinee_names: Vec<Symbol>,
    arms: Vec<Vec<elaborated::Pattern>>,
    next: Vec<lambda::Expr>,
) -> lambda::ExprKind {
    let mut result = vec![];

    for (patterns, next) in arms.iter().zip(next.into_iter()) {
        let mut result_binders = vec![];

        for (scrutinee, pat) in scrutinee_names.iter().zip(patterns.iter()) {
            let var = Box::new(ExprKind::Variable(scrutinee.clone()));
            for (binder, name) in pattern_binders(var.clone(), &*pat) {
                result_binders.push((name, ExprKind::Access(binder)));
            }
        }

        result.push(
            result_binders
                .into_iter()
                .fold(next, |next, (name, occ)| {
                    Box::new(ExprKind::Let(name, Box::new(occ), next))
                }),
        )
    }

    let scrutinee = scrutinee_names.iter().map(|x| Box::new(lambda::ExprKind::Variable(x.clone()))).collect();
    let pat = pattern::compile(scrutinee, arms);

    match pat {
        lambda::Tree::Fail => todo!(),
        lambda::Tree::Leaf(x, _) => {
            *result[x].clone()
        },
        lambda::Tree::Switch(_, _) => {
            ExprKind::Switch(
                scrutinee_names
                    .iter()
                    .map(|x| Box::new(ExprKind::Variable(x.clone())))
                    .collect(),
                pat,
                result,
            )
        },
    }
}

pub fn compile_match(
    ctx: &mut Ctx,
    scrutinee: Vec<elaborated::Expr<Type<Real>>>,
    arms: Vec<Vec<elaborated::Pattern>>,
    next: Vec<lambda::Expr>,
) -> lambda::ExprKind {
    let scrutinee_names = (0..scrutinee.len())
        .map(|_| ctx.new_var_name())
        .collect::<Vec<_>>();

    let result = compile_match_with_names(scrutinee_names.clone(), arms, next);

    *scrutinee_names
        .iter()
        .zip(scrutinee.transform(ctx).into_iter())
        .fold(Box::new(result), |next, (name, value)| {
            Box::new(ExprKind::Let(name.clone(), value, next))
        })
}

impl Ctx {
    pub fn new_var_name(&mut self) -> Symbol {
        let name = format!("_v{}", self.var);
        self.var += 1;
        Symbol::intern(&name)
    }
}

pub trait Transform {
    type Out;

    fn transform(self, ctx: &mut Ctx) -> Self::Out;
}

impl<T: Transform> Transform for Box<T> {
    type Out = Box<T::Out>;

    fn transform(self, ctx: &mut Ctx) -> Self::Out {
        Box::new((*self).transform(ctx))
    }
}

impl<T: Transform> Transform for Vec<T> {
    type Out = Vec<T::Out>;

    fn transform(self, ctx: &mut Ctx) -> Self::Out {
        self.into_iter().map(|x| x.transform(ctx)).collect()
    }
}

impl<T: Transform> Transform for Option<T> {
    type Out = Option<T::Out>;

    fn transform(self, ctx: &mut Ctx) -> Self::Out {
        self.map(|x| x.transform(ctx))
    }
}

impl<T: Transform> Transform for HashMap<Qualified, T> {
    type Out = HashMap<Qualified, T::Out>;

    fn transform(self, ctx: &mut Ctx) -> Self::Out {
        self.into_iter()
            .map(|(k, v)| (k, v.transform(ctx)))
            .collect()
    }
}

impl Transform for Vec<elaborated::Statement<Type<Real>>> {
    type Out = lambda::ExprKind;

    fn transform(self, ctx: &mut Ctx) -> Self::Out {

        fn fold(x: Vec<elaborated::Statement<Type<Real>>>, ctx: &mut Ctx) -> lambda::ExprKind {
            let (fst, tail) = x.split_first().unwrap();

            match fst.clone() {
                elaborated::Statement::Let(x) => {
                    let pats = vec![vec![x.pattern]];
                    let next = vec![Box::new(fold(tail.to_vec(), ctx))];
                    let scrutinee = vec![x.expr];
                    compile_match(ctx, scrutinee, pats, next)
                }
                elaborated::Statement::Expr(x) => {
                    let expr_kind = Box::new(*x.data.transform(ctx));
                    if tail.is_empty() {
                        *expr_kind
                    } else {
                        ExprKind::Seq(expr_kind, Box::new(fold(tail.to_vec(), ctx)))
                    }
                }
                elaborated::Statement::Error => unreachable!(),
            }
        }

        fold(self, ctx)
    }
}

impl<T: Transform> Transform for (Symbol, T) {
    type Out = (Symbol, T::Out);

    fn transform(self, ctx: &mut Ctx) -> Self::Out {
        (self.0, self.1.transform(ctx))
    }
}

impl<T: Transform> Transform for Spanned<T> {
    type Out = T::Out;

    fn transform(self, ctx: &mut Ctx) -> Self::Out {
        self.data.transform(ctx)
    }
}

impl Transform for elaborated::ExprKind<Type<Real>> {
    type Out = lambda::ExprKind;

    fn transform(self, ctx: &mut Ctx) -> Self::Out {
        use lambda::ExprKind::*;

        match self {
            elaborated::ExprKind::Lambda(x) => {
                let name = ctx.new_var_name();

                let t = x.body.transform(ctx);
                let res = compile_match_with_names(vec![name.clone()], vec![vec![x.param]], vec![t]);

                Lambda(
                    name.clone(),
                    Box::new(res),
                )
            }
            elaborated::ExprKind::Application(x) => {
                if let elaborated::ExprKind::Constructor(z, y) = *x.func.data {
                    Constructor(z, y, x.args.transform(ctx))
                } else {
                    Application(x.func.transform(ctx), x.args.transform(ctx))
                }
            }
            elaborated::ExprKind::Variable(x) => Variable(x),
            elaborated::ExprKind::Constructor(x, y) => Constructor(x, y, vec![]),
            elaborated::ExprKind::Function(x, _) => Function(x),
            elaborated::ExprKind::Projection(x) => Projection(x.field, x.expr.transform(ctx)),
            elaborated::ExprKind::Let(x) => {
                let patterns = vec![vec![x.pattern]];
                
                let scrutinee = vec![x.body];

                let next = vec![x.next.transform(ctx)];

                compile_match(ctx, scrutinee, patterns, next)
            }
            elaborated::ExprKind::When(x) => {
                let rest = x.arms.into_iter().map(|x| (x.patterns, x.expr));

                let (switches, exprs): (Vec<_>, Vec<_>) = rest.unzip();

                let next = exprs.transform(ctx);
                compile_match(ctx, x.scrutinee, switches, next)
            }
            elaborated::ExprKind::Do(x) => x.transform(ctx),
            elaborated::ExprKind::Literal(x) => Literal(x),
            elaborated::ExprKind::RecordInstance(x) => {
                RecordInstance(x.name, x.fields.transform(ctx))
            }
            elaborated::ExprKind::RecordUpdate(x) => {
                RecordUpdate(x.qualified, x.expr.transform(ctx), x.fields.transform(ctx))
            }
            elaborated::ExprKind::Tuple(x) => Tuple(x.exprs.transform(ctx)),
            elaborated::ExprKind::Error => todo!(),
        }
    }
}

impl Transform for elaborated::LetDecl<Type<Real>> {
    type Out = lambda::LetDecl;

    fn transform(self, ctx: &mut Ctx) -> Self::Out {
        let (patterns, next) : (Vec<_>, Vec<_>) = self.body.into_iter().map(|x| (x.patterns, x.expr)).unzip();
        let scrutinee_names = (0..patterns[0].len()).map(|_| ctx.new_var_name()).collect::<Vec<_>>();

        let next = next.transform(ctx);
        let result = compile_match_with_names(scrutinee_names.clone(), patterns, next);

        let binder_names = (0..self.binders.len()).map(|_| ctx.new_var_name()).collect::<Vec<_>>();
        let binders = self.binders.into_iter().map(|x| x.0).collect::<Vec<_>>();

        let result = compile_match_with_names(binder_names.clone(), vec![binders], vec![Box::new(result)]);

        let all_names = binder_names.into_iter().chain(scrutinee_names.into_iter()).collect::<Vec<_>>();

        lambda::LetDecl {
            name: self.name.clone(),
            binders: all_names,
            body: Box::new(result),
            constants: self.constants.clone()
        }
    }
}

impl Transform for elaborated::ExternalDecl<Type<Real>> {
    type Out = lambda::ExternalDecl;

    fn transform(self, _ctx: &mut Ctx) -> Self::Out {
        lambda::ExternalDecl {
            name: self.name.clone(),
            binding: self.binding,
        }
    }
}

impl Transform for elaborated::TypeDecl {
    type Out = lambda::TypeDecl;

    fn transform(self, _ctx: &mut Ctx) -> Self::Out {
        use lambda::TypeDecl::*;
        match self {
            elaborated::TypeDecl::Abstract => Abstract,
            elaborated::TypeDecl::Enum(x) => Enum(x),
            elaborated::TypeDecl::Record(x) => Record(x),
        }
    }
}

impl Transform for elaborated::Program<Type<Real>> {
    type Out = lambda::Program;

    fn transform(self, ctx: &mut Ctx) -> Self::Out {
        lambda::Program {
            lets: self.lets.transform(ctx),
            types: self.types.transform(ctx),
            externals: self.externals.transform(ctx),
        }
    }
}
