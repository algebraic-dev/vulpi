use std::{collections::HashMap, mem};

use petgraph::{graph::DiGraph, stable_graph::NodeIndex};
use vulpi_intern::Symbol;
use vulpi_syntax::{
    lambda::{self, LetDecl, Program},
    r#abstract::Qualified,
};

pub struct Context {
    nodes: HashMap<Qualified, NodeIndex>,
    graph: DiGraph<(), ()>,
    current: Qualified,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            nodes: HashMap::new(),
            graph: DiGraph::new(),
            current: Qualified {
                path: Symbol::intern(""),
                name: Symbol::intern(""),
            },
        }
    }
}

pub trait Check {
    fn check(&mut self, ctx: &mut Context);
}

impl Check for lambda::ExprKind {
    fn check(&mut self, ctx: &mut Context) {
        match self {
            lambda::ExprKind::Lambda(_, expr) => expr.check(ctx),
            lambda::ExprKind::Application(expr, args) => {
                expr.check(ctx);
                for arg in args {
                    arg.check(ctx);
                }
            }
            lambda::ExprKind::Variable(_) => {}
            lambda::ExprKind::Constructor(c) | lambda::ExprKind::Function(c) => {
                let node = ctx
                    .nodes
                    .entry(c.clone())
                    .or_insert_with(|| ctx.graph.add_node(()))
                    .clone();
                let current = ctx
                    .nodes
                    .entry(ctx.current.clone())
                    .or_insert_with(|| ctx.graph.add_node(()));

                ctx.graph.add_edge(*current, node, ());
            }
            lambda::ExprKind::Object(_, args) => {
                for arg in args {
                    arg.check(ctx);
                }
            }
            lambda::ExprKind::Projection(_, expr) => expr.check(ctx),
            lambda::ExprKind::Access(expr, _) => expr.check(ctx),
            lambda::ExprKind::Block(stmts) => {
                for stmt in stmts {
                    match stmt {
                        lambda::Stmt::Let(_, expr) => expr.check(ctx),
                        lambda::Stmt::Expr(expr) => expr.check(ctx),
                    }
                }
            }
            lambda::ExprKind::Literal(_) => {}
            lambda::ExprKind::RecordInstance(_, args) => {
                for (_, arg) in args {
                    arg.check(ctx);
                }
            }
            lambda::ExprKind::RecordUpdate(_, expr, args) => {
                expr.check(ctx);
                for (_, arg) in args {
                    arg.check(ctx);
                }
            }
            lambda::ExprKind::Tuple(args) => {
                for arg in args {
                    arg.check(ctx);
                }
            }
            lambda::ExprKind::Switch(_, _, actions) => {
                for action in actions {
                    action.check(ctx);
                }
            }
        }
    }
}

impl Check for LetDecl {
    fn check(&mut self, ctx: &mut Context) {
        self.body.check(ctx);
    }
}

impl Check for Program {
    fn check(&mut self, ctx: &mut Context) {
        for (name, decl) in &mut self.lets {
            ctx.current = name.clone();
            ctx.nodes
            .entry(ctx.current.clone())
            .or_insert_with(|| ctx.graph.add_node(()));
        decl.check(ctx);
        }
    }
}

impl Check for Vec<Program> {
    fn check(&mut self, ctx: &mut Context) {
        for program in self {
            program.check(ctx);
        }
    }
}

pub fn is_constant(expr: &lambda::Expr) -> bool {
    match &**expr {
        lambda::ExprKind::Lambda(_, _) => false,
        _ => true,
    }
}

pub fn has_no_side_effects(expr: &lambda::Expr) -> bool {
    match &**expr {
        lambda::ExprKind::Lambda(_, _) => true,
        lambda::ExprKind::Application(_, _) => false,
        lambda::ExprKind::Variable(_) => true,
        lambda::ExprKind::Constructor(_) => true,
        lambda::ExprKind::Function(_) => true,
        lambda::ExprKind::Object(_, _) => true,
        lambda::ExprKind::Projection(_, expr) => has_no_side_effects(expr),
        lambda::ExprKind::Access(expr, _) => has_no_side_effects(expr),
        lambda::ExprKind::Block(_) => true,
        lambda::ExprKind::Literal(_) => true,
        lambda::ExprKind::RecordInstance(_, _) => false,
        lambda::ExprKind::RecordUpdate(_, _, _) => false,
        lambda::ExprKind::Tuple(args) => args.iter().all(has_no_side_effects),
        lambda::ExprKind::Switch(_, _, _) => false,
    }
}

pub fn remove_lets(program: &mut Program, ctx: &mut Context) {
    program.lets = mem::take(&mut program.lets)
        .into_iter()
        .filter(|(name, body)| {
            let node = ctx.nodes.get(name).unwrap();
            ctx.graph
                .neighbors_directed(*node, petgraph::Direction::Incoming)
                .count()
                != 0
                || (is_constant(&body.body) && body.is_in_source_code)
                || !has_no_side_effects(&body.body)
        })
        .collect();
}

pub fn dead_code_remove(programs: &mut Vec<Program>) {
    let mut ctx = Context::default();
    programs.check(&mut ctx);

    for program in programs {
        remove_lets(program, &mut ctx);
    }
}
