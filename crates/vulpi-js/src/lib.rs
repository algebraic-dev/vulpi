use std::{borrow::Cow, vec, collections::HashMap};

use petgraph::graph::DiGraph;
use resast::{
    decl::VarDecl,
    expr::*,
    pat::Pat,
    stmt::{Stmt, SwitchCase, SwitchStmt},
    Func, FuncArg, FuncBody, Ident, ProgramPart, decl::Decl, VarKind, Program,
};
use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_syntax::{elaborated::LiteralKind, lambda, r#abstract::Qualified};

/// The context used to generate new variable names and other things.
#[derive(Default, Clone)]
pub struct Context<'a> {
    upwards: Vec<Stmt<'a>>,
    scope: Vec<usize>,
    externals: HashMap<Qualified, Symbol>,
}

impl<'a> Context<'a> {
    pub fn add_upwards(&mut self, stmt: Stmt<'a>) {
        self.upwards.push(stmt);
    }

    pub fn take_upwards(&mut self) -> Vec<Stmt<'a>> {
        let start = self.scope.last().cloned().unwrap_or(0);
        self.upwards.drain(start..).collect()
    }

    pub fn scope<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.scope.push(self.upwards.len());
        let result = f(self);
        self.scope.pop();
        result
    }
}

pub trait Transform {
    type Out<'a>;

    fn transform<'a>(self, ctx: &mut Context<'a>) -> Self::Out<'a>;
}

impl<T: Transform> Transform for Vec<T> {
    type Out<'a> = Vec<T::Out<'a>>;

    fn transform<'a>(self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        self.into_iter().map(|x| x.transform(ctx)).collect()
    }
}

impl<T: Transform> Transform for Box<T> {
    type Out<'a> = Box<T::Out<'a>>;

    fn transform<'a>(self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        Box::new((*self).transform(ctx))
    }
}

impl<T: Transform> Transform for Option<T> {
    type Out<'a> = Option<T::Out<'a>>;

    fn transform<'a>(self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        self.map(|x| x.transform(ctx))
    }
}

fn pat_ident<'a>(ident: Symbol) -> Pat<'a> {
    Pat::Ident(Ident::new(ident.get()))
}

impl Transform for lambda::Stmt {
    type Out<'a> = Stmt<'a>;

    fn transform<'a>(self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        match self {
            lambda::Stmt::Let(name, val) => {
                let val = *val.transform(ctx);
                Stmt::Var(vec![VarDecl {
                    id: pat_ident(name),
                    init: Some(val),
                }])
            }
            lambda::Stmt::Expr(e) => {
                let e = *e.transform(ctx);
                Stmt::Expr(e)
            }
        }
    }
}

impl Transform for (lambda::TagType, lambda::Case) {
    type Out<'a> = Expr<'a>;

    fn transform<'a>(self, _ctx: &mut Context<'a>) -> Self::Out<'a> {
        match self {
            (lambda::TagType::Number(id), lambda::Case::Tuple(_)) => Expr::Lit(resast::expr::Lit::Number(
                std::borrow::Cow::Owned(id.to_string()),
            )),
            (lambda::TagType::Field(id), lambda::Case::Constructor(_, _)) |
            (lambda::TagType::Number(id), lambda::Case::Constructor(_, _)) => Expr::Lit(resast::expr::Lit::Number(
                Cow::Owned(id.to_string()),
            )),
            (lambda::TagType::Number(_), lambda::Case::Literal(l)) => match &*l {
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
                LiteralKind::Unit => Expr::Lit(Lit::Number(Cow::Owned("0".to_string()))),
            },
            _ => unreachable!()
        }
    }
}

impl Transform for lambda::ExprKind {
    type Out<'a> = Expr<'a>;

    fn transform<'a>(self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        match self {
            lambda::ExprKind::Lambda(symbols, expr) => {
                let (result, mut upwards) = ctx.scope(|ctx| {
                    let result = *expr.transform(ctx);
                    (result, ctx.take_upwards())
                });

                if upwards.is_empty() {
                    Expr::Func(Func {
                        id: None,
                        params: symbols
                            .into_iter()
                            .map(|x| FuncArg::Pat(pat_ident(x)))
                            .collect(),
                        body: FuncBody(vec![ProgramPart::Stmt(Stmt::Return(Some(result)))]),
                        generator: false,
                        is_async: false,
                    })
                } else {
                    upwards.push(Stmt::Return(Some(result)));

                    Expr::Func(Func {
                        id: None,
                        params: symbols
                            .into_iter()
                            .map(|x| FuncArg::Pat(pat_ident(x)))
                            .collect(),
                        body: FuncBody(upwards.into_iter().map(ProgramPart::Stmt).collect()),
                        generator: false,
                        is_async: false,
                    })
                }
            }
            lambda::ExprKind::Application(callee, args) => {
                let callee = *callee.transform(ctx);
                let args = args.transform(ctx);

                Expr::Call(CallExpr {
                    callee: Box::new(callee),
                    arguments: args.into_iter().map(|x| *x).collect(),
                })
            }
            lambda::ExprKind::Variable(name) => Expr::Ident(Ident::new(name.get())),
            lambda::ExprKind::Constructor(cons) => Expr::Ident(Ident::new(cons.mangle())),
            lambda::ExprKind::Function(x) => {
                if let Some(symbol) = ctx.externals.get(&x) {
                    Expr::Ident(Ident::new(symbol.get()))
                } else {
                    Expr::Ident(Ident::new(x.mangle()))
                }
            },
            lambda::ExprKind::Object(id, args) => Expr::Call(CallExpr {
                callee: Box::new(Expr::Ident(Ident::new("obj".to_string()))),
                arguments: vec![
                    Expr::Lit(Lit::Number(Cow::Owned(id.to_string()))),
                    Expr::Array(args.transform(ctx).into_iter().map(|x| Some(*x)).collect()),
                ],
            }),
            lambda::ExprKind::Projection(field, obj) => Expr::Member(MemberExpr {
                computed: false,
                object: Box::new(*obj.transform(ctx)),
                property: Box::new(Expr::Ident(Ident::new(field.name.get()))),
            }),
            lambda::ExprKind::Access(obj, place) => Expr::Member(MemberExpr {
                computed: true,
                object: Box::new(*obj.transform(ctx)),
                property: Box::new(Expr::Lit(Lit::Number(Cow::Owned(place.to_string())))),
            }),
            lambda::ExprKind::Block(statements) => {

                let size = statements.len() - 1;
                for (i, statement) in statements.into_iter().enumerate() {
                    let is_last = i == size;
                    if !is_last {
                        let statement = statement.transform(ctx);
                        ctx.add_upwards(statement);
                    } else if let lambda::Stmt::Expr(e) = statement {
                        return *e.transform(ctx);
                    }
                }

                return Expr::Lit(Lit::Number(Cow::Owned("0".to_string())));
            }
            lambda::ExprKind::Literal(l) => match &*l {
                LiteralKind::String(str) => {
                    Expr::Lit(Lit::String(StringLit::Double(Cow::Owned(str.get()))))
                }
                LiteralKind::Integer(int) => Expr::Lit(Lit::Number(Cow::Owned(int.get()))),
                LiteralKind::Float(flt) => Expr::Lit(Lit::Number(Cow::Owned(flt.get()))),
                LiteralKind::Char(chr) => Expr::Lit(Lit::String(StringLit::Double(Cow::Owned(
                    chr.get().to_string(),
                )))),
                LiteralKind::Unit => Expr::Lit(Lit::Number(Cow::Owned("0".to_string()))),
            },
            lambda::ExprKind::RecordInstance(_, fields) => Expr::Obj(
                fields
                    .into_iter()
                    .map(|(name, value)| {
                        ObjProp::Prop(Prop {
                            key: PropKey::Lit(Lit::String(StringLit::Double(Cow::Owned(
                                name.get(),
                            )))),
                            value: PropValue::Expr(*value.transform(ctx)),
                            kind: resast::PropKind::Init,
                            method: false,
                            computed: false,
                            short_hand: false,
                            is_static: false,
                        })
                    })
                    .collect(),
            ),
            lambda::ExprKind::RecordUpdate(_, object, fields) => {
                let args: Vec<_> = fields
                    .into_iter()
                    .map(|(name, value)| {
                        ObjProp::Prop(Prop {
                            key: resast::expr::PropKey::Lit(Lit::String(
                                resast::expr::StringLit::Double(Cow::Owned(name.get())),
                            )),
                            value: resast::expr::PropValue::Expr(*value.transform(ctx)),
                            kind: resast::PropKind::Init,
                            method: false,
                            computed: false,
                            short_hand: false,
                            is_static: false,
                        })
                    })
                    .collect();

                let mut fields = vec![ObjProp::Spread(*object.transform(ctx))];
                fields.extend(args);
                resast::expr::Expr::Obj(fields)
            }
            lambda::ExprKind::Tuple(elements) => Expr::Array(
                elements
                    .transform(ctx)
                    .into_iter()
                    .map(|x| Some(*x))
                    .collect(),
            ),
            lambda::ExprKind::Switch(name, tree, actions) => {
                fn compile_switch<'a>(
                    to_set: Expr<'a>,
                    switch: lambda::Tree,
                    context: &mut Context<'a>,
                    actions: &[lambda::Expr],
                ) -> Stmt<'a> {
                    match switch {
                        lambda::Tree::Leaf(x) => {
                            context.scope(|context| {
                                    
                                let result = actions[x].clone().transform(context);
                                let mut upwards = context.take_upwards();

                                upwards.push(Stmt::Expr(Expr::Assign(AssignExpr {
                                    operator: resast::AssignOp::Equal,
                                    left: AssignLeft::Expr(Box::new(to_set)),
                                    right: Box::new(*result),
                                })));


                                Stmt::Block(resast::stmt::BlockStmt(
                                    upwards.into_iter().map(ProgramPart::Stmt).collect(),
                                ))
                            })
                        }
                        lambda::Tree::Switch(scrutinee, branches) => {
                            let mut compiled_branches = vec![];
                            let mut tests = vec![];

                            for (case, tag, tree) in branches {
                                let accessor = get_tag_accessor(tag.clone(), &scrutinee, context);
                                tests.push(Box::new(accessor));

                                let test = Box::new((tag, case).transform(context));

                                compiled_branches.push(SwitchCase {
                                    test: Some(*test),
                                    consequent: vec![
                                        ProgramPart::Stmt(compile_switch(
                                            to_set.clone(),
                                            tree,
                                            context,
                                            actions,
                                        )),
                                        ProgramPart::Stmt(Stmt::Break(None)),
                                    ],
                                })
                            }

                            Stmt::Switch(SwitchStmt {
                                discriminant: *tests[0].clone(),
                                cases: compiled_branches
                            })
                        }
                    }
                }

                ctx.add_upwards(Stmt::Var(vec![VarDecl {
                    id: pat_ident(name.clone()),
                    init: None,
                }]));

                let sttm = compile_switch(Expr::Ident(Ident::new(name.get())), tree, ctx, &actions);

                ctx.add_upwards(sttm);

                Expr::Ident(Ident::new(name.get()))
            }
        }
    }
}

fn get_tag_accessor<'a>(
    tag: lambda::TagType,
    scrutinee: &Box<lambda::ExprKind>,
    context: &mut Context<'a>,
) -> Expr<'a> {
    match tag {
        lambda::TagType::Field(_) => Expr::Member(MemberExpr {
            computed: false,
            object: Box::new(*scrutinee.clone().transform(context)),
            property: Box::new(Expr::Ident(Ident::new("tag".to_string()))),
        }),
        lambda::TagType::Number(_) => *scrutinee.clone().transform(context),
        lambda::TagType::Size => todo!(),
        lambda::TagType::None => todo!(),
    }
}

impl Transform for lambda::LetDecl {
    type Out<'a> = Decl<'a>;

    fn transform<'a>(self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        match *self.body {
            lambda::ExprKind::Lambda(param, body) => {
                let transform = body.transform(ctx);
                let mut upwards = ctx.take_upwards();
                upwards.push(Stmt::Return(Some(*transform)));

                Decl::Func(Func {
                    id: Some(Ident::new(self.name.clone().mangle())),
                    params: param.iter().map(|x| FuncArg::Pat(pat_ident(x.clone()))).collect(),
                    body: FuncBody(upwards.into_iter().map(ProgramPart::Stmt).collect()),
                    generator: false,
                    is_async: false,
                })
            }
            body => {
                let body = body.transform(ctx);
                Decl::Var(VarKind::Let, vec![VarDecl {
                    id: pat_ident(Symbol::intern(&self.name.clone().mangle())),
                    init: Some(body),
                }])
            }
        }
    }
}

impl Transform for lambda::Program {
    type Out<'a> = Vec<(Qualified, Vec<ProgramPart<'a>>, Option<HashMap<Qualified, Span>>)>;

    fn transform<'a>(self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        let mut decls = vec![];
        
        for (_, let_decl) in self.lets {
            let name = let_decl.name.clone();
            let hash_map = let_decl.constants.clone();
            let decl = let_decl.transform(ctx);
            let mut new_decls = ctx.take_upwards().into_iter().map(ProgramPart::Stmt).collect::<Vec<_>>();
            new_decls.push(ProgramPart::Decl(decl));
            decls.push((name, new_decls, hash_map));
        }
        
        decls
    }
}
pub struct Programs(pub Vec<lambda::Program>);

impl Transform for Programs {
    type Out<'a> = Program<'a>;

    fn transform<'a>(self, ctx: &mut Context<'a>) -> Self::Out<'a> {
        let mut decls = HashMap::new();
        let mut petgraph = DiGraph::new();
        let mut nodes = HashMap::new();
        let mut parts = Vec::new();

        for program in &self.0 {
            for (name, symbol) in &program.externals {
                ctx.externals.insert(name.clone(), symbol.clone());
            }
        }

        for program in &self.0 {
            for (result, command) in &program.commands {
                if command.get() == "javascript" {
                    let js = ressa::Parser::new(&result.get_static()).unwrap();
                    for part in js.flatten() {
                        parts.push(part.clone());
                    }
                }
            }
        }
        
        for program in self.0 {
            for (name, decl, dependencies) in program.transform(ctx) {
                let from = nodes.entry(name.clone()).or_insert_with(|| {
                    petgraph.add_node(())
                }).clone();

                if let Some(dependencies) = dependencies {
                    for (to_, _) in dependencies {
                        let to = nodes.entry(to_.clone()).or_insert_with(|| {
                            petgraph.add_node(())
                        });
                        petgraph.add_edge(from, *to, ());
                    }
                }

                decls.insert(name, decl);
            } 
        }

        let top_ = petgraph::algo::toposort(&petgraph, None).unwrap();
        let inv_map = nodes.iter().map(|(k, v)| (v, k)).collect::<HashMap<_, _>>();

        let ordered_expr = top_.iter().rev().filter_map(|x| {
            decls.get(&inv_map[x].clone()).cloned()
        }).flatten().collect::<Vec<_>>();

        Program::Script(parts.into_iter().chain(ordered_expr.into_iter()).collect())
    }
}