use std::{borrow::Cow, collections::HashMap};

use resast::{
    decl::{Decl, VarDecl},
    expr::{BinaryExpr, CallExpr, Expr},
    stmt::Stmt,
    FuncArg, Ident, ProgramPart, Func, FuncBody,
};
use vulpi_intern::Symbol;
use vulpi_syntax::{
    elaborated::LiteralKind,
    lambda::{self, ExternalDecl, LetDecl, Tree},
    r#abstract::Qualified,
};

#[derive(Default)]
pub struct Context<'a> {
    externals: HashMap<Qualified, Symbol>,
    upwards: Vec<(Symbol, Expr<'a>)>,
}

pub trait Transform {
    type Out<'a>;

    fn transform<'a>(&self, ctx: &mut Context<'a>) -> Self::Out<'a>;
}

impl Transform for lambda::Case {
    type Out<'a> = Expr<'a>;

    fn transform<'a>(&self, _ctx: &mut Context<'a>) -> Self::Out<'a> {
        match self {
            lambda::Case::Tuple(x) => Expr::Lit(resast::expr::Lit::Number(
                std::borrow::Cow::Owned(x.to_string()),
            )),
            lambda::Case::Constructor(name, _) => Expr::Lit(resast::expr::Lit::String(
                resast::expr::StringLit::Single(Cow::Owned(name.to_string())),
            )),
            lambda::Case::Literal(l) => match &**l {
                LiteralKind::String(x) => Expr::Lit(resast::expr::Lit::String(
                    resast::expr::StringLit::Single(Cow::Owned(x.get())),
                )),
                LiteralKind::Integer(x) => {
                    Expr::Lit(resast::expr::Lit::Number(std::borrow::Cow::Owned(x.get())))
                }
                LiteralKind::Float(x) => {
                    Expr::Lit(resast::expr::Lit::Number(std::borrow::Cow::Owned(x.get())))
                }
                LiteralKind::Char(x) => Expr::Lit(resast::expr::Lit::String(
                    resast::expr::StringLit::Single(Cow::Owned(x.get())),
                )),
                LiteralKind::Unit => Expr::Lit(resast::expr::Lit::String(
                    resast::expr::StringLit::Single(Cow::Owned("()".to_string())),
                )),
            },
        }
    }
}

impl Transform for lambda::Occurrence {
    type Out<'a> = Expr<'a>;

    fn transform<'a>(&self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        fn transform_index<'a>(
            index: &[lambda::Index],
            last: Expr<'a>,
            ctx: &mut Context,
        ) -> Expr<'a> {
            if let Some((fst, tail)) = index.split_first() {
                let last = match fst {
                    lambda::Index::Cons(x) => Expr::Member(resast::expr::MemberExpr {
                        object: Box::new(last),
                        property: Box::new(Expr::Lit(resast::expr::Lit::Number(
                            std::borrow::Cow::Owned(x.to_string()),
                        ))),
                        computed: true,
                    }),
                    lambda::Index::Tuple(x) => Expr::Member(resast::expr::MemberExpr {
                        object: Box::new(last),
                        property: Box::new(Expr::Lit(resast::expr::Lit::Number(
                            std::borrow::Cow::Owned(x.to_string()),
                        ))),
                        computed: true,
                    }),
                };

                transform_index(tail, last, ctx)
            } else {
                last
            }
        }

        transform_index(&self.1, self.0.transform(ctx), ctx)
    }
}

impl Transform for lambda::ExprKind {
    type Out<'a> = resast::expr::Expr<'a>;

    fn transform<'a>(&self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        match self {
            lambda::ExprKind::Lambda(name, expr) => {
                let x = expr.transform(ctx);

                let upwards = std::mem::take(&mut ctx.upwards);

                let body = if upwards.is_empty() {
                    resast::expr::ArrowFuncBody::Expr(Box::new(x))
                } else {
                    resast::expr::ArrowFuncBody::FuncBody(resast::FuncBody(
                        upwards
                            .into_iter()
                            .map(|(name, expr)| {
                                ProgramPart::Decl(resast::decl::Decl::Var(
                                    resast::VarKind::Let,
                                    vec![VarDecl {
                                        id: resast::pat::Pat::Ident(Ident::new(name.get())),
                                        init: Some(expr.clone()),
                                    }],
                                ))
                            })
                            .collect(),
                    ))
                };

                resast::expr::Expr::ArrowFunc(resast::expr::ArrowFuncExpr {
                    id: None,
                    params: vec![FuncArg::Pat(resast::pat::Pat::Ident(Ident::new(
                        name.get(),
                    )))],
                    body,
                    expression: false,
                    generator: false,
                    is_async: false,
                })
            }
            lambda::ExprKind::Application(f, a) => {
                a.into_iter().fold(f.transform(ctx), |acc, x| {
                    resast::expr::Expr::Call(CallExpr {
                        callee: Box::new(acc),
                        arguments: vec![x.transform(ctx)],
                    })
                })
            }
            lambda::ExprKind::Variable(x) => resast::expr::Expr::Ident(Ident::new(x.get())),
            lambda::ExprKind::Constructor(_, n, a) => {
                let vec = vec![Some(Expr::Lit(resast::expr::Lit::String(
                    resast::expr::StringLit::Single(Cow::Owned(n.to_string())),
                )))];

                let args = a.iter().map(|x| Some(x.transform(ctx))).collect::<Vec<_>>();

                let all = vec.into_iter().chain(args).collect::<Vec<_>>();

                resast::expr::Expr::Array(all)
            }
            lambda::ExprKind::Function(x) => {
                if let Some(x) = ctx.externals.get(x) {
                    resast::expr::Expr::Ident(Ident::new(x.get()))
                } else {
                    resast::expr::Expr::Ident(Ident::new(x.to_string()))
                }
            }
            lambda::ExprKind::Projection(_, _) => todo!(),
            lambda::ExprKind::Let(name, value, next) => {
                let transform = value.transform(ctx);
                ctx.upwards.push((name.clone(), transform));
                next.transform(ctx)
            }
            lambda::ExprKind::Seq(before, after) => {
                let before = before.transform(ctx);
                let after = after.transform(ctx);

                resast::expr::Expr::Sequence(vec![before, after])
            }
            lambda::ExprKind::Literal(l) => match &**l {
                LiteralKind::String(x) => Expr::Lit(resast::expr::Lit::String(
                    resast::expr::StringLit::Single(Cow::Owned(x.get())),
                )),
                LiteralKind::Integer(x) => {
                    Expr::Lit(resast::expr::Lit::Number(std::borrow::Cow::Owned(x.get())))
                }
                LiteralKind::Float(x) => {
                    Expr::Lit(resast::expr::Lit::Number(std::borrow::Cow::Owned(x.get())))
                }
                LiteralKind::Char(x) => Expr::Lit(resast::expr::Lit::String(
                    resast::expr::StringLit::Single(Cow::Owned(x.get())),
                )),
                LiteralKind::Unit => Expr::Lit(resast::expr::Lit::String(
                    resast::expr::StringLit::Single(Cow::Owned("()".to_string())),
                )),
            },
            lambda::ExprKind::RecordInstance(_, _) => todo!(),
            lambda::ExprKind::RecordUpdate(_, _, _) => todo!(),
            lambda::ExprKind::Access(x) => x.transform(ctx),
            lambda::ExprKind::Tuple(_) => todo!(),
            lambda::ExprKind::Switch(_, tree, actions) => {
                fn compile_switch<'a>(
                    switch: Tree,
                    ctx: &mut Context<'a>,
                    actions: &[lambda::Expr],
                ) -> resast::expr::Expr<'a> {
                    match switch {
                        Tree::Fail => unreachable!(),
                        Tree::Leaf(x, _) => actions[x].transform(ctx),
                        Tree::Switch(occ, cases) => {
                            if cases.len() == 1 {
                                compile_switch(cases[0].1.clone(), ctx, actions)
                            } else {
                                let number = resast::expr::Lit::Number(std::borrow::Cow::Owned(
                                    "0".to_string(),
                                ));
                                cases.iter().fold(Expr::Lit(number), |acc, (mot, t)| {
                                    Expr::Conditional(resast::expr::ConditionalExpr {
                                        test: Box::new(Expr::Binary(BinaryExpr {
                                            left: Box::new(Expr::Member(
                                                resast::expr::MemberExpr {
                                                    object: Box::new(occ.transform(ctx)),
                                                    property: Box::new(Expr::Lit(
                                                        resast::expr::Lit::Number(
                                                            std::borrow::Cow::Owned(
                                                                "0".to_string(),
                                                            ),
                                                        ),
                                                    )),
                                                    computed: true,
                                                },
                                            )),
                                            operator: resast::BinaryOp::Equal,
                                            right: Box::new(mot.transform(ctx)),
                                        })),
                                        alternate: Box::new(acc),
                                        consequent: Box::new(compile_switch(
                                            t.clone(),
                                            ctx,
                                            actions,
                                        )),
                                    })
                                })
                            }
                        }
                    }
                }

                compile_switch(tree.clone(), ctx, actions)
            }
        }
    }
}

impl Transform for LetDecl {
    type Out<'a> = Decl<'a>;

    fn transform<'a>(&self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        let x = self.body.transform(ctx);

        let upwards = std::mem::take(&mut ctx.upwards);

        let body = if upwards.is_empty() {
            vec![ProgramPart::Stmt(Stmt::Return(Some(x.clone())))]
        } else {
            let mut collect: Vec<_> = upwards
                .into_iter()
                .map(|(name, expr)| {
                    ProgramPart::Decl(resast::decl::Decl::Var(
                        resast::VarKind::Let,
                        vec![VarDecl {
                            id: resast::pat::Pat::Ident(Ident::new(name.get())),
                            init: Some(expr.clone()),
                        }],
                    ))
                })
                .collect();

            collect.push(ProgramPart::Stmt(Stmt::Return(Some(x.clone()))));

            collect
        };

        if let Some((last, tail)) = self.binders.split_first() {

            let mut mem = body;

            for name in tail {
                mem = vec![ProgramPart::Stmt(Stmt::Return(Some(resast::expr::Expr::Func(Func {
                    id: None,
                    params: vec![FuncArg::Pat(resast::pat::Pat::Ident(Ident::new(
                        name.get(),
                    )))],
                    body: FuncBody(mem),
                    generator: false,
                    is_async: false,
                }))))]
            }

            resast::decl::Decl::Func(Func {
                id: Some(Ident::new(self.name.to_string())),
                params: vec![FuncArg::Pat(resast::pat::Pat::Ident(Ident::new(
                    last.get(),
                )))],
                body: FuncBody(mem),
                generator: false,
                is_async: false,
            })
        } else {
            Decl::Var(
                resast::VarKind::Let,
                vec![VarDecl {
                    id: resast::pat::Pat::Ident(Ident::new(self.name.to_string())),
                    init: Some(x),
                }],
            )
        }
    }
}

impl Transform for ExternalDecl {
    type Out<'a> = ();

    fn transform<'a>(&self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        ctx.externals
            .insert(self.name.clone(), self.binding.clone());
    }
}

impl Transform for lambda::Program {
    type Out<'a> = resast::Program<'a>;

    fn transform<'a>(&self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        for external in self.externals.values() {
            external.transform(ctx);
        }

        let expr = self
            .lets
            .values()
            .map(|x| x.transform(ctx))
            .collect::<Vec<_>>();

        let part = expr
            .into_iter()
            .map(|x| resast::ProgramPart::Decl(x))
            .collect();

        resast::Program::Script(part)
    }
}
