use vulpi_syntax::{
    elaborated::{ExprKind, *},
    r#abstract::Qualified,
};
use vulpi_typer::r#type::TypeKind;

use crate::{
    ir::{self, FnDecl},
    pattern::{extract_accesors, CaseTree, Problem},
    Context,
};

type Type = vulpi_typer::Type<vulpi_typer::Real>;

pub trait FirstPass {
    type Output;

    fn first_pass(self, context: &mut Context) -> Self::Output;
}

impl FirstPass for Program<Type> {
    type Output = Vec<FnDecl>;

    fn first_pass(self, context: &mut Context) -> Self::Output {
        for typ in self.types {
            match typ.1 {
                TypeDecl::Enum(variants) => {
                    context.enums.insert(typ.0.clone(), variants.clone());
                    for (i, (variant, size)) in variants.iter().enumerate() {
                        context
                            .constructors
                            .insert(variant.clone(), (typ.0.clone(), i, *size));
                    }
                }
                TypeDecl::Record(fields) => {
                    context.records.insert(typ.0.clone(), fields.clone());
                    for (i, field) in fields.iter().enumerate() {
                        context.fields.insert(field.clone(), (typ.0.clone(), i));
                    }
                }
                _ => (),
            }
        }

        for effs in self.effects {
            context.eff_types.insert(effs.0.clone(), effs.1.clone());

            for (i, eff) in effs.1.into_iter().enumerate() {
                context.effects.insert(eff, (effs.0.clone(), i));
            }
        }

        self.lets
            .into_iter()
            .map(|decl| decl.first_pass(context))
            .collect()
    }
}

impl FirstPass for (Qualified, LetDecl<Type>) {
    type Output = FnDecl;

    fn first_pass(self, ctx: &mut Context) -> Self::Output {
        let (name, decl) = self;

        let mut params = vec![];
        let mut accessors = vec![];

        for (binder, _) in decl.binders {
            match *binder {
                PatternKind::Wildcard => params.push(ctx.fresh()),
                PatternKind::Variable(x) => params.push(x),
                _ => {
                    let value = ctx.fresh();
                    params.push(value.clone());
                    let expr = ir::ExprKind::Variable(value);
                    let new_accessors = extract_accesors(&binder, Box::new(expr));
                    accessors.extend(new_accessors);
                }
            }
        }

        let tree = Problem::new(&decl.body);

        let body = tree.compile();

        let arms: Vec<_> = decl
            .body
            .into_iter()
            .map(|x| (x.patterns, x.expr))
            .collect();

        let mut bodies = vec![];

        let arm_params = arms.first().map(|x| x.0.len()).unwrap_or_default();
        let arm_params = (0..arm_params).map(|_| ctx.fresh()).collect::<Vec<_>>();

        for (patterns, body) in arms {
            let mut acessors = vec![];

            for (pattern, param) in patterns.iter().zip(arm_params.iter()) {
                let new_accessors =
                    extract_accesors(pattern, Box::new(ir::ExprKind::Variable(param.clone())));
                acessors.extend(new_accessors);
            }

            let body = body.first_pass(ctx);

            let mut accessors: Vec<_> = acessors
                .into_iter()
                .map(|x| ir::Statement::Let(x.0, x.1))
                .collect();

            accessors.push(ir::Statement::Expr(body));

            let block = ir::ExprKind::Block(accessors);
            bodies.push(Box::new(block));
        }

        let mut block = accessors
            .into_iter()
            .map(|(x, y)| ir::Statement::Let(x, y))
            .collect::<Vec<_>>();

        if let CaseTree::Leaf(x) = body {
            block.push(ir::Statement::Expr(bodies.swap_remove(x)))
        } else {
            block.push(ir::Statement::Expr(Box::new(ir::ExprKind::Tree(
                body, bodies,
            ))));
        }

        let body = Box::new(ir::ExprKind::Block(block));

        FnDecl { name, params, body }
    }
}

impl FirstPass for Literal {
    type Output = ir::ExprKind;

    fn first_pass(self, context: &mut Context) -> Self::Output {
        match *self {
            LiteralKind::String(s) => ir::ExprKind::Literal(ir::Literal::String(s)),
            LiteralKind::Integer(i) => ir::ExprKind::Literal(ir::Literal::Integer(i)),
            LiteralKind::Float(f) => ir::ExprKind::Literal(ir::Literal::Float(f)),
            LiteralKind::Char(c) => ir::ExprKind::Literal(ir::Literal::Char(c)),
            LiteralKind::Unit => ir::ExprKind::Tuple(vec![]),
        }
    }
}

impl FirstPass for StatementKind<Type> {
    type Output = Vec<ir::Statement>;

    fn first_pass(self, context: &mut Context) -> Self::Output {
        match self {
            StatementKind::Let(l) => match *l.pattern {
                PatternKind::Wildcard => vec![ir::Statement::Expr(l.expr.first_pass(context))],
                PatternKind::Variable(s) => vec![ir::Statement::Let(s, l.expr.first_pass(context))],
                _ => {
                    let name = context.fresh();

                    let scrutinee = Box::new(ir::ExprKind::Variable(name.clone()));
                    let accessors = extract_accesors(&l.pattern, scrutinee);

                    let first = ir::Statement::Let(name, l.expr.first_pass(context));

                    let accessors = accessors
                        .into_iter()
                        .map(|x| ir::Statement::Let(x.0, x.1))
                        .collect::<Vec<_>>();

                    let mut block = vec![first];
                    block.extend(accessors);

                    block
                }
            },
            StatementKind::Expr(expr) => vec![ir::Statement::Expr(expr.first_pass(context))],
            StatementKind::Error => unreachable!(),
        }
    }
}

impl FirstPass for Expr<Type> {
    type Output = ir::Expr;

    fn first_pass(self, ctx: &mut Context) -> Self::Output {
        match *self {
            ExprKind::Lambda(_) => todo!(),
            ExprKind::Application(x) => {
                let mut args = vec![];

                for arg in x.args {
                    args.push(arg.first_pass(ctx));
                }

                let to_add = arrow_spine(x.typ);

                let params = (0..to_add).map(|_| ctx.fresh()).collect::<Vec<_>>();

                for param in params.clone() {
                    args.push(Box::new(ir::ExprKind::Variable(param.clone())));
                }

                let result = match *x.func {
                    ExprKind::Constructor(_, name) => {
                        let (_, i, _) = ctx.constructors.get(&name).unwrap().clone();

                        let tag = Box::new(ir::ExprKind::Tag(name, i));

                        let args = std::iter::once(tag).chain(args.into_iter()).collect();

                        Box::new(ir::ExprKind::Tuple(args))
                    }
                    ExprKind::Function(name, _) => Box::new(ir::ExprKind::Function(name, args)),
                    _ => {
                        let func = x.func.first_pass(ctx);
                        Box::new(ir::ExprKind::Application(func, args))
                    }
                };

                if !params.is_empty() {
                    Box::new(ir::ExprKind::Lambda(params, result))
                } else {
                    result
                }
            }
            ExprKind::Variable(n) => Box::new(ir::ExprKind::Variable(n)),
            ExprKind::Constructor(_, name) => {
                let (_, i, size) = ctx.constructors.get(&name).unwrap().clone();

                let params = (0..size).map(|_| ctx.fresh()).collect::<Vec<_>>();

                let tag = Box::new(ir::ExprKind::Tag(name, i));

                let args: Vec<_> = params
                    .iter()
                    .map(|x| Box::new(ir::ExprKind::Variable(x.clone())))
                    .collect();

                let result: Vec<_> = std::iter::once(tag).chain(args.into_iter()).collect();

                let result = Box::new(ir::ExprKind::Tuple(result));

                if !params.is_empty() {
                    Box::new(ir::ExprKind::Lambda(params, result))
                } else {
                    result
                }
            }
            ExprKind::Function(name, _) => Box::new(ir::ExprKind::FunPtr(name)),
            ExprKind::When(when) => {
                let problem = Problem::new(&when.arms);

                let tree = problem.compile();

                let mut exprs = Vec::new();

                let scrutinee: Vec<_> = when.scrutinee.iter().map(|_| ctx.fresh()).collect();

                let mut bodies: Vec<_> = when
                    .scrutinee
                    .into_iter()
                    .zip(scrutinee.iter())
                    .map(|(x, name)| ir::Statement::Let(name.clone(), x.first_pass(ctx)))
                    .collect();

                for arm in when.arms {
                    let mut statements = Vec::new();

                    for (i, pat) in arm.patterns.into_iter().enumerate() {
                        let scrutinee = Box::new(ir::ExprKind::Variable(scrutinee[i].clone()));

                        let accessors = extract_accesors(&pat, scrutinee);

                        let accessors = accessors
                            .into_iter()
                            .map(|x| ir::Statement::Let(x.0, x.1))
                            .collect::<Vec<_>>();

                        statements.extend(accessors);
                    }

                    statements.push(ir::Statement::Expr(arm.expr.first_pass(ctx)));

                    exprs.push(Box::new(ir::ExprKind::Block(statements)))
                }

                bodies.push(ir::Statement::Expr(Box::new(ir::ExprKind::Tree(
                    tree, exprs,
                ))));

                Box::new(ir::ExprKind::Block(bodies))
            }
            ExprKind::Do(block) => {
                let block = block
                    .into_iter()
                    .flat_map(|x| x.first_pass(ctx))
                    .collect::<Vec<_>>();

                Box::new(ir::ExprKind::Block(block))
            }
            ExprKind::Literal(lit) => Box::new(lit.first_pass(ctx)),
            ExprKind::Effect(_, _) => todo!(),
            ExprKind::Let(_) => todo!(),
            ExprKind::Projection(proj) => {
                let (name, place) = ctx.fields.get(&proj.field).unwrap();
                todo!()
            }
            ExprKind::RecordInstance(_) => todo!(),
            ExprKind::RecordUpdate(_) => todo!(),
            ExprKind::Tuple(_) => todo!(),
            ExprKind::Handler(_) => todo!(),
            ExprKind::Cases(_) => todo!(),
            ExprKind::Error => unreachable!(),
        }
    }
}

pub fn arrow_spine(mut typ: Type) -> usize {
    let mut count = 0;

    loop {
        match typ.as_ref() {
            TypeKind::Arrow(l) => {
                count += 1;
                typ = l.body.clone();
            }
            TypeKind::Forall(n) => {
                typ = n.body.clone();
            }
            TypeKind::Exists(n) => {
                typ = n.body.clone();
            }
            _ => break count,
        }
    }
}
