use std::{collections::{HashMap, HashSet}, rc::Rc, cell::RefCell};

use petgraph::{stable_graph::NodeIndex, graph::DiGraph, visit::EdgeRef};
use vulpi_intern::Symbol;
use vulpi_show::Show;
use vulpi_syntax::{lambda::{self, LetDecl, Program}, r#abstract::Qualified};

pub struct Context<'a> {
    vars: HashMap<Qualified, Vec<&'a mut lambda::ExprKind>>,
    nodes: HashMap<Qualified, NodeIndex>,
    graph: DiGraph<(), ()>,
    should_inline: HashMap<Qualified, lambda::ExprKind>,
    current: Qualified,
    changed: bool
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        Context {
            vars: HashMap::new(),
            nodes: HashMap::new(),
            graph: DiGraph::new(),
            current: Qualified {
                path: Symbol::intern(""),
                name: Symbol::intern(""),
            },
            should_inline: HashMap::new(),
            changed: false
        }
    }
}

pub trait Transform {
    type Out;

    fn transform<'a>(&'a mut self, ctx: &mut Context<'a>) -> Self::Out;
}

impl Transform for lambda::ExprKind {
    type Out = ();

    fn transform<'a>(&'a mut self, ctx: &mut Context<'a>) {
        match self {
            lambda::ExprKind::Lambda(_, expr) => expr.transform(ctx),
            lambda::ExprKind::Application(expr, args) => {
                expr.transform(ctx);
                for arg in args {
                    arg.transform(ctx);
                }
            }
            lambda::ExprKind::Variable(_) => {}
            lambda::ExprKind::Constructor(c) |
            lambda::ExprKind::Function(c) => {
                let node = ctx.nodes.entry(c.clone()).or_insert_with(|| ctx.graph.add_node(())).clone();
                let current = ctx.nodes.entry(ctx.current.clone()).or_insert_with(|| ctx.graph.add_node(()));

                ctx.graph.add_edge(*current, node, ());

                ctx.vars.entry(c.clone()).or_default().push(self);
            }
            lambda::ExprKind::Object(_, args) => {
                for arg in args {
                    arg.transform(ctx);
                }
            }
            lambda::ExprKind::Projection(_, expr) => expr.transform(ctx),
            lambda::ExprKind::Access(expr, _) => expr.transform(ctx),
            lambda::ExprKind::Block(stmts) => {
                for stmt in stmts {
                    match stmt {
                        lambda::Stmt::Let(_, expr) => expr.transform(ctx),
                        lambda::Stmt::Expr(expr) => expr.transform(ctx),
                    }
                }
            }
            lambda::ExprKind::Literal(_) => {}
            lambda::ExprKind::RecordInstance(_, args) => {
                for (_, arg) in args {
                    arg.transform(ctx);
                }
            }
            lambda::ExprKind::RecordUpdate(_, expr, args) => {
                expr.transform(ctx);
                for (_, arg) in args {
                    arg.transform(ctx);
                }
            }
            lambda::ExprKind::Tuple(args) => {
                for arg in args {
                    arg.transform(ctx);
                }
            }
            lambda::ExprKind::Switch(_, _, actions) => {
                for action in actions {
                    action.transform(ctx);
                }
            }
        }
    }
}

impl Transform for LetDecl {
    type Out = ();

    fn transform<'a>(&'a mut self, ctx: &mut Context<'a>) {
        self.body.transform(ctx);        
    }
}

impl Transform for Program {
    type Out = ();

    fn transform<'a>(&'a mut self, ctx: &mut Context<'a>) {
        for (name, decl) in &mut self.lets {
            ctx.nodes.entry(ctx.current.clone()).or_insert_with(|| ctx.graph.add_node(()));

            ctx.current = name.clone();
            
            if should_inline(&decl.body) {
                ctx.should_inline.insert(name.clone(), *decl.body.clone());
            }

            decl.transform(ctx);
        }
    }
}

impl Transform for Vec<Program> {
    type Out = ();
    
    fn transform<'a>(&'a mut self, ctx: &mut Context<'a>) {
        for program in self {
            program.transform(ctx);
        }
    }
}

pub fn traverse<F: Fn(&mut lambda::ExprKind) -> ()>(expr: &mut lambda::ExprKind, f: Rc<F>) {
    f(expr);

    match expr {
        lambda::ExprKind::Lambda(_, body) => {
            traverse(body, f)
        },
        lambda::ExprKind::Application(func, args) => {
            traverse(func, f.clone());
            for arg in args {
                traverse(arg, f.clone());
            }
        }
        lambda::ExprKind::Variable(_) => {}
        lambda::ExprKind::Constructor(_) |
        lambda::ExprKind::Function(_) => {}
        lambda::ExprKind::Object(_, args) => {
            for arg in args {
                traverse(arg, f.clone());
            }
        }
        lambda::ExprKind::Projection(_, expr) => traverse(expr, f.clone()),
        lambda::ExprKind::Access(expr, _) => traverse(expr, f.clone()),
        lambda::ExprKind::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    lambda::Stmt::Let(_, expr) => traverse(expr, f.clone()),
                    lambda::Stmt::Expr(expr) => traverse(expr, f.clone()),
                }
            }
        }
        lambda::ExprKind::Literal(_) => {}
        lambda::ExprKind::RecordInstance(_, args) => {
            for (_, arg) in args {
                traverse(arg, f.clone());
            }
        }
        lambda::ExprKind::RecordUpdate(_, expr, args) => {
            traverse(expr, f.clone());
            for (_, arg) in args {
                traverse(arg, f.clone());
            }
        }
        lambda::ExprKind::Tuple(args) => {
            for arg in args {
                traverse(arg, f.clone());
            }
        }
        lambda::ExprKind::Switch(_, _, actions) => {
            for action in actions {
                traverse(action, f.clone());
            }
        }
    
    }
}

pub fn traverse_programs<F: Fn(&mut lambda::ExprKind) -> ()>(programs: &mut Vec<Program>, f: F) {
    let f = Rc::new(f);
    for program in programs {
        for (_, decl) in &mut program.lets {
            traverse(&mut decl.body, f.clone());
        }
    }
}

pub fn is_complex(expr: &lambda::ExprKind) -> bool {
    match expr {
        lambda::ExprKind::Application(_, _) => true,
        lambda::ExprKind::Constructor(_) => false,
        lambda::ExprKind::Variable(_) => false,
        lambda::ExprKind::Function(_) => false,
        lambda::ExprKind::Object(_, _) => true,
        lambda::ExprKind::Lambda(_, body) => is_complex(body),
        lambda::ExprKind::Projection(_, _) => true,
        lambda::ExprKind::Access(_, _) => true,
        lambda::ExprKind::Literal(_) => false,
        
        lambda::ExprKind::RecordInstance(_, fields) => fields.iter().any(|(_, x)| is_complex(x)),
        lambda::ExprKind::RecordUpdate(_, _, fields) => fields.iter().any(|(_, x)| is_complex(x)),
        lambda::ExprKind::Tuple(fields) => fields.iter().any(|x| is_complex(x)),
        
        lambda::ExprKind::Block(_) => true,
        lambda::ExprKind::Switch(_, _, _) => false,
    }
}

pub fn apply(expr: &mut lambda::ExprKind, changed: Rc<RefCell<bool>>) {
    match expr {
        lambda::ExprKind::Application(func, args) => {
            match &mut **func { 
                lambda::ExprKind::Lambda(params,ref mut body) => {
                    let mut subs = im_rc::HashMap::new();

                    for (param, arg) in params.into_iter().zip(args) {
                        subs.insert(param.clone(), *arg.clone());
                    }
                    
                    substitute( body, subs);

                    *expr = *body.clone();

                    *changed.borrow_mut() = true;

                    traverse(expr, Rc::new(|x: &mut _| {
                        apply(x, changed.clone());
                    }));
                }
                _ => ()
            }
        }
        _ => ()
    }
}

pub fn substitute(expr: &mut lambda::ExprKind, mut subs: im_rc::HashMap<Symbol, lambda::ExprKind>) {
    match expr {
        lambda::ExprKind::Lambda(params, body) => {
            for param in params {
                subs.remove(param);
            }
            substitute(body, subs);
        }
        lambda::ExprKind::Application(func, args) => {
            substitute(func, subs.clone());
            for arg in args {
                substitute(arg, subs.clone());
            }
        }
        lambda::ExprKind::Variable(name) => {
            if let Some(sub) = subs.get(name) {
                *expr = sub.clone();
            }
        }
        lambda::ExprKind::Constructor(_) => {}
        lambda::ExprKind::Function(_) => {}
        lambda::ExprKind::Object(_, args) => {
            for arg in args {
                substitute(arg, subs.clone());
            }
        }
        lambda::ExprKind::Projection(_, expr) => {
            substitute(expr, subs);
        }
        lambda::ExprKind::Access(expr, _) => {
            substitute(expr, subs);
        }
        lambda::ExprKind::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    lambda::Stmt::Let(_, expr) => substitute(expr, subs.clone()),
                    lambda::Stmt::Expr(expr) => substitute(expr, subs.clone()),
                }
            }
        }
        lambda::ExprKind::Literal(_) => {}
        lambda::ExprKind::RecordInstance(_, fields) => {
            for (_, arg) in fields {
                substitute(arg, subs.clone());
            }
        }
        lambda::ExprKind::RecordUpdate(_, expr, fields) => {
            substitute(expr, subs.clone());
            for (_, arg) in fields {
                substitute(arg, subs.clone());
            }
        }
        lambda::ExprKind::Tuple(fields) => {
            for arg in fields {
                substitute(arg, subs.clone());
            }
        }
        lambda::ExprKind::Switch(_, _, actions) => {
            for action in actions {
                substitute(action, subs.clone());
            }
        }
    }
}

pub fn are_complex(exprs: &[Box<lambda::ExprKind>]) -> bool {
    exprs.iter().any(|x| is_complex(x))
}

pub fn should_inline(expr: &lambda::ExprKind) -> bool {
    match expr {
        lambda::ExprKind::Application(func, args) => {
            !is_complex(func) && !are_complex(args)
        }
        lambda::ExprKind::Constructor(_) => true,
        lambda::ExprKind::Variable(_) => true,
        lambda::ExprKind::Function(_) => true,
        lambda::ExprKind::Literal(_) => true,
        lambda::ExprKind::Object(_, args) => !are_complex(args), 
        lambda::ExprKind::Lambda(_, body) => should_inline(body),
        lambda::ExprKind::Projection(_, e) => !is_complex(e),
        lambda::ExprKind::Access(e, _) => !is_complex(e),
        
        lambda::ExprKind::RecordInstance(_, fields) => !fields.iter().any(|(_, x)| is_complex(x)),
        lambda::ExprKind::RecordUpdate(_, e, fields) => !is_complex(e) && !fields.iter().any(|(_, x)| is_complex(x)),
        lambda::ExprKind::Tuple(fields) => !fields.iter().any(|x| is_complex(x)),
        
        lambda::ExprKind::Block(_) => false,
        lambda::ExprKind::Switch(_, _, _) => false,
    }
}

pub fn remove_recursive_inline_marks(ctx: &mut Context) {
    'breaker: loop {
        let loops = petgraph::algo::tarjan_scc(&ctx.graph);
        let inv_map = ctx.nodes.iter().map(|(k, v)| (*v, k.clone())).collect::<HashMap<_, _>>();

        for node in ctx.graph.node_indices() {
            if ctx.graph.contains_edge(node, node) {
                ctx.should_inline.remove(&inv_map[&node]);
            }
        }
        
        for loop_ in loops {
            if loop_.len() > 1 {
                for node in loop_ {
                    let node_rem = ctx.should_inline.remove(&inv_map[&node]);
                    if node_rem.is_none() {
                        let cloned = ctx.graph.edges_directed(node, petgraph::Direction::Incoming).map(|x| x.id()).collect::<Vec<_>>();
                        for edge in cloned {
                            ctx.graph.remove_edge(edge);
                        }
                        continue 'breaker
                    }
                }
            }
        }

        break
    }
}

pub fn inline(programs: &mut Vec<Program>) {
    loop {
        let changed = {
            let mut ctx = Default::default();
            programs.transform(&mut ctx);
    
            remove_recursive_inline_marks(&mut ctx);
    
            for (name, value) in ctx.should_inline {
                if let Some(exprs) = ctx.vars.get_mut(&name) {
                    for expr in exprs {
                        **expr = value.clone();
                        ctx.changed = true;
                    }
                }
            }
    
            ctx.changed
        };

        let changed = Rc::new(RefCell::new(changed));

        traverse_programs(programs, |x| {
            apply(x, changed.clone())
        });

        if !*changed.borrow() {
            break;
        }
    }
}