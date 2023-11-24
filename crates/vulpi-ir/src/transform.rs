//! This module compiles a TypedTree to a Vulpi IR tree called Lambda. This is the first step in
//! lowering the AST to a form that is easier to work with for code generation.

use std::{cell::RefCell, collections::HashMap, rc::Rc, vec};

use vulpi_intern::Symbol;

use vulpi_syntax::{
    elaborated::*,
    lambda::{self, Case, ConsDef, Stmt, TagType},
    r#abstract::Qualified,
};

use crate::pattern;
use vulpi_typer::{real::Real, Type};

#[derive(Clone)]
pub enum TypeDef {
    NewType,
    Enumerated,
    Heavy,
    Tuple,
    Abstract,
    Record,
}

pub enum Flag {
    InHead,
    InTail,
    InBlock,
}

/// The context used to generate new variable names and other things.
#[derive(Default, Clone)]
pub struct Context {
    counter: Rc<RefCell<usize>>,
    upwards: Rc<RefCell<Vec<lambda::Stmt>>>,
    scoped: Rc<RefCell<Vec<usize>>>,
    constructors: Rc<RefCell<HashMap<Qualified, (ConsDef, usize)>>>,
    vars: im_rc::HashMap<Symbol, usize>,
    types: im_rc::HashMap<Qualified, TypeDef>,
}

impl Context {
    pub fn new_var(&mut self, name: String) -> Symbol {
        let mut counter = self.counter.borrow_mut();
        let id = *counter;
        *counter += 1;
        Symbol::intern(&format!("{}${}", name, id))
    }

    pub fn add_var(&mut self, name: Symbol) -> Symbol {
        let i = self
            .vars
            .entry(name.clone())
            .and_modify(|x| *x += 1)
            .or_insert(0);
        if *i == 0 {
            name
        } else {
            Symbol::intern(&format!("{}${}", name.get(), i))
        }
    }

    pub fn add_var_local(&self, name: Symbol) -> Context {
        let mut context = self.clone();
        context.add_var(name);
        context
    }

    pub fn find_var(&self, name: Symbol) -> Symbol {
        let mut name = name;
        if let Some(count) = self.vars.get(&name) {
            if *count > 0 {
                name = Symbol::intern(&format!("{}${}", name.get(), count));
            }
        }
        name
    }

    pub fn add_upwards(&mut self, expr: lambda::Stmt) {
        self.upwards.borrow_mut().push(expr);
    }

    pub fn drain_upwards(&mut self) -> Vec<lambda::Stmt> {
        let start = self.scoped.borrow().last().cloned().unwrap_or_default();
        self.upwards.borrow_mut().drain(start..).collect()
    }

    pub fn scope<T>(&mut self, f: impl FnOnce(&mut Context) -> T) -> T {
        self.scoped.borrow_mut().push(self.upwards.borrow().len());
        let res = f(&mut self.clone());
        self.scoped.borrow_mut().pop();
        res
    }

    pub fn has_upwards(&self) -> bool {
        !self.upwards.borrow().is_empty()
    }

    pub fn add_constructor(&mut self, name: Qualified, cons: ConsDef, size: usize) {
        self.constructors.borrow_mut().insert(name, (cons, size));
    }

    pub fn get_constructor(&self, name: &Qualified) -> ConsDef {
        self.constructors.borrow().get(name).cloned().unwrap().0
    }

    pub fn is_newtype(&self, name: &Qualified) -> bool {
        if let Some(cons) = self.constructors.borrow().get(name) {
            matches!(cons, (ConsDef::NewType, 1))
        } else {
            false
        }
    }
}

fn translate_occurence(occ: pattern::Occurrence) -> lambda::Expr {
    occ.1.into_iter().fold(occ.0, |acc, x| match x {
        pattern::Index::Cons(i) => Box::new(lambda::ExprKind::Access(acc, i)),
        pattern::Index::Tuple(i) => Box::new(lambda::ExprKind::Access(acc, i)),
    })
}

fn translate_case_to_tagged_expr(context: &mut Context, case: Case) -> TagType {
    match case {
        Case::Constructor(name, _) => match context.get_constructor(&name) {
            ConsDef::Enumerated(_, i) => TagType::Number(i),
            ConsDef::Heavy(_, i, _) => TagType::Field(i),
            ConsDef::NewType => TagType::None,
            ConsDef::Tuple => TagType::None,
        },
        Case::Literal(_) => TagType::None,
        Case::Tuple(_) => TagType::Size,
    }
}

fn generate_pattern_name(context: &mut Context, pat: &Pattern) -> (Symbol, bool) {
    match &**pat {
        PatternKind::Wildcard => (Symbol::intern("_"), false),
        PatternKind::Variable(x) => (context.add_var(x.clone()), false),
        PatternKind::Application(app) if context.is_newtype(&app.func) => {
            generate_pattern_name(context, &app.args[0])
        }
        _ => (context.new_var("v".to_string()), true),
    }
}

fn generate_scrutinee_name(context: &mut Context, expr: &lambda::Expr) -> (Symbol, bool) {
    match &**expr {
        lambda::ExprKind::Variable(x) => (context.find_var(x.clone()), false),
        _ => (context.new_var("v".to_string()), true),
    }
}

fn translate_tree(
    context: &mut Context,
    tree: pattern::Tree,
    actions: Vec<lambda::Expr>,
) -> lambda::Expr {
    fn translate(context: &mut Context, tree: pattern::Tree) -> lambda::Tree {
        match tree {
            pattern::Tree::Fail => unreachable!(),
            pattern::Tree::Leaf(i, _) => lambda::Tree::Leaf(i),
            pattern::Tree::Switch(occ, cases) => {
                if cases.len() == 1 {
                    translate(context, cases[0].1.clone())
                } else {
                    let branches = cases
                        .into_iter()
                        .map(|(case, tree)| {
                            (
                                case.clone(),
                                translate_case_to_tagged_expr(context, case),
                                translate(context, tree),
                            )
                        })
                        .collect();

                    lambda::Tree::Switch(translate_occurence(occ), branches)
                }
            }
        }
    }

    match tree {
        pattern::Tree::Fail => unreachable!(),
        pattern::Tree::Leaf(i, _) => actions[i].clone(),
        pattern::Tree::Switch(_, _) => {
            let tree = translate(context, tree);
            Box::new(lambda::ExprKind::Switch(
                context.new_var("r".to_string()),
                tree,
                actions,
            ))
        }
    }
}
fn compile_binders_without_names(
    context: &mut Context,
    scrutinee_names: Vec<Symbol>,
    patterns: Vec<Pattern>,
) {
    for (scrutinee, pat) in scrutinee_names.iter().zip(patterns) {
        // We assume that all the scrutines are variables right now.
        let scrutinee = lambda::ExprKind::Variable(scrutinee.clone());
        for (binder, name) in pattern::pattern_binders(Box::new(scrutinee), &pat) {
            // If binder is not redundant, then we need to add a let binding.
            if binder.1.len() != 0 {
                let translate_occurence = translate_occurence(binder);
                context.add_upwards(Stmt::Let(name, translate_occurence))
            }
        }
    }
}

fn compile_binders(
    context: &mut Context,
    scrutinee: Vec<Expr<Type<Real>>>,
    patterns: Vec<Pattern>,
) {
    let scrutinee = scrutinee
        .into_iter()
        .map(|x| x.transform(context))
        .collect::<Vec<_>>();

    let (scrutinee_names, _): (Vec<_>, Vec<_>) = patterns
        .iter()
        .map(|x| generate_pattern_name(context, x))
        .unzip();

    for (name, scrutinee) in scrutinee_names.iter().zip(scrutinee) {
        context.add_upwards(Stmt::Let(name.clone(), scrutinee));
    }

    compile_binders_without_names(context, scrutinee_names, patterns);
}

fn compile_match_with_names(
    context: &mut Context,
    scrutinee_names: Vec<Symbol>,
    arms: Vec<Vec<Pattern>>,
    actions: Vec<Expr<Type<Real>>>,
) -> lambda::Expr {
    // Vector of all the result actions with let bindings.
    let mut result_actions = vec![];

    for (patterns, action) in arms.iter().zip(actions) {
        let mut statements = vec![];

        for (scrutinee, pat) in scrutinee_names.iter().zip(patterns) {
            // We assume that all the scrutines are variables right now.
            let scrutinee = lambda::ExprKind::Variable(scrutinee.clone());
            for (binder, name) in pattern::pattern_binders(Box::new(scrutinee), &pat) {
                // If binder is not redundant, then we need to add a let binding.
                if binder.1.len() != 0 || arms.len() > 1 {
                    let translate_occurence = translate_occurence(binder);
                    statements.push(Stmt::Let(name, translate_occurence))
                }
            }
        }

        let action = action.transform(context);

        statements.extend(context.drain_upwards());

        // If it's empty then we should not add a block
        if statements.is_empty() {
            result_actions.push(action);
        } else {
            statements.push(Stmt::Expr(action));
            result_actions.push(Box::new(lambda::ExprKind::Block(statements)));
        }
    }

    let scrutinee = scrutinee_names
        .iter()
        .map(|x| Box::new(lambda::ExprKind::Variable(x.clone())))
        .collect::<Vec<_>>();

    let compiled_tree = pattern::compile(scrutinee, arms);

    let t = translate_tree(context, compiled_tree, result_actions);

    t
}

// Compiles a pattern match expression into a [lambda::Expr].
fn compile_match(
    context: &mut Context,
    scrutinee: Vec<Expr<Type<Real>>>,
    arms: Vec<Vec<Pattern>>,
    actions: Vec<Expr<Type<Real>>>,
) -> lambda::Expr {
    let scrutinee = scrutinee
        .into_iter()
        .map(|x| x.transform(context))
        .collect::<Vec<_>>();

    let (scrutinee_names, should_create_let): (Vec<_>, Vec<_>) = scrutinee
        .iter()
        .map(|x| generate_scrutinee_name(context, x))
        .unzip();

    for ((name, should_create_let), scrutinee) in
        scrutinee_names.iter().zip(should_create_let).zip(scrutinee)
    {
        if should_create_let {
            context.add_upwards(Stmt::Let(name.clone(), scrutinee));
        }
    }

    context.scope(|context| compile_match_with_names(context, scrutinee_names, arms, actions))
}

pub trait Transform {
    type Out;
    fn transform<'a>(&self, context: &mut Context) -> Self::Out;
}

impl Transform for SttmKind<Type<Real>> {
    type Out = ();

    fn transform<'a>(&self, context: &mut Context) -> Self::Out {
        match self {
            SttmKind::Let(let_) => {
                let arms = vec![let_.pattern.clone()];
                let scrutinee = vec![let_.expr.clone()];

                compile_binders(context, scrutinee, arms);
            }
            SttmKind::Expr(e) => {
                let transform = e.transform(context);
                context.add_upwards(Stmt::Expr(transform));
            }
            SttmKind::Error => unreachable!(),
        }
    }
}

impl<T: Transform> Transform for Vec<T> {
    type Out = Vec<T::Out>;

    fn transform<'a>(&self, context: &mut Context) -> Self::Out {
        self.iter().map(|x| x.transform(context)).collect()
    }
}

impl Transform for Expr<Type<Real>> {
    type Out = lambda::Expr;

    fn transform<'a>(&self, context: &mut Context) -> Self::Out {
        match &*self.data {
            ExprKind::Lambda(lambda) => context.scope(|context| {
                let arms = vec![lambda.param.clone()];
                let scrutinee = vec![generate_pattern_name(context, &lambda.param).0];
                compile_binders_without_names(context, scrutinee.clone(), arms);

                context.scope(|context| {
                    let mut upwards = context.drain_upwards();
                    let body = lambda.body.transform(context);

                    if upwards.is_empty() {
                        Box::new(lambda::ExprKind::Lambda(scrutinee, body))
                    } else {
                        upwards.push(Stmt::Expr(body));
                        Box::new(lambda::ExprKind::Lambda(
                            scrutinee,
                            Box::new(lambda::ExprKind::Block(upwards)),
                        ))
                    }
                })
            }),
            ExprKind::Application(app) => {
                let func = app.func.transform(context);
                let arg = app.args.transform(context);
                Box::new(lambda::ExprKind::Application(func, vec![arg]))
            }
            ExprKind::Variable(var) => {
                Box::new(lambda::ExprKind::Variable(context.find_var(var.clone())))
            }
            ExprKind::Constructor(_, name) => Box::new(lambda::ExprKind::Constructor(name.clone())),
            ExprKind::Function(name, _) => Box::new(lambda::ExprKind::Function(name.clone())),

            ExprKind::Projection(field) => Box::new(lambda::ExprKind::Projection(
                field.field.clone(),
                field.expr.transform(context),
            )),
            ExprKind::Let(let_expr) => {
                let arms = vec![let_expr.pattern.clone()];
                let scrutinee = vec![let_expr.body.clone()];

                compile_binders(context, scrutinee, arms);
                let_expr.next.transform(context)
            }
            ExprKind::When(when_expr) => {
                let (actions, patterns): (Vec<_>, Vec<_>) = when_expr
                    .arms
                    .clone()
                    .into_iter()
                    .map(|x| (x.expr, x.patterns))
                    .unzip();

                compile_match(context, when_expr.scrutinee.clone(), patterns, actions)
            }
            ExprKind::Do(sttms) => context.scope(|context| {
                sttms.into_iter().for_each(|x| x.transform(context));
                let statements = context.drain_upwards();
                Box::new(lambda::ExprKind::Block(statements))
            }),
            ExprKind::Literal(lit) => Box::new(lambda::ExprKind::Literal(lit.clone())),
            ExprKind::RecordInstance(instance) => {
                let mut fields = vec![];
                for (name, expr) in instance.fields.iter() {
                    fields.push((name.clone(), expr.transform(context)));
                }
                Box::new(lambda::ExprKind::RecordInstance(
                    instance.name.clone(),
                    fields,
                ))
            }
            ExprKind::RecordUpdate(update) => {
                let mut fields = vec![];
                for (name, expr) in update.fields.iter() {
                    fields.push((name.clone(), expr.transform(context)));
                }
                Box::new(lambda::ExprKind::RecordUpdate(
                    update.name.clone(),
                    update.expr.transform(context),
                    fields,
                ))
            }
            ExprKind::Tuple(t) => {
                let t = t.exprs.transform(context);
                Box::new(lambda::ExprKind::Tuple(t))
            }
            ExprKind::Error => unreachable!(),
        }
    }
}

impl Transform for (Qualified, LetDecl<Type<Real>>) {
    type Out = lambda::LetDecl;

    fn transform<'a>(&self, context: &mut Context) -> Self::Out {
        let mut context = context.clone();

        let new_names = self.1.body[0].patterns.iter()
            .map(|x| {
                if self.1.body.len() == 1 {
                    generate_pattern_name(&mut context, x).0
                } else {
                    context.new_var("v".to_string())
                }
            })
            .collect::<Vec<_>>();

        let binders = self
            .1
            .binders
            .iter()
            .map(|x| generate_pattern_name(&mut context, &x.0).0)
            .collect::<Vec<_>>();

        compile_binders_without_names(
            &mut context,
            binders.clone(),
            self.1.binders.iter().map(|x| x.0.clone()).collect(),
        );

        let (actions, patterns): (Vec<_>, Vec<_>) = self
            .1
            .body
            .iter()
            .map(|x| (x.expr.clone(), x.patterns.clone()))
            .unzip();

        let expr = compile_match_with_names(&mut context, new_names.clone(), patterns, actions);

        let mut upwards = context.drain_upwards();

        if upwards.is_empty() {
            lambda::LetDecl {
                name: self.0.clone(),
                body: binders
                    .into_iter()
                    .chain(new_names)
                    .rfold(expr, |acc, name| {
                        Box::new(lambda::ExprKind::Lambda(vec![name], acc))
                    }),
                constants: self.1.constants.clone(),
            }
        } else {
            upwards.push(Stmt::Expr(expr));

            lambda::LetDecl {
                name: self.0.clone(),
                body: binders
                    .into_iter()
                    .chain(new_names)
                    .rfold(Box::new(lambda::ExprKind::Block(upwards)), |acc, name| {
                        Box::new(lambda::ExprKind::Lambda(vec![name], acc))
                    }),
                constants: self.1.constants.clone(),
            }
        }
    }
}

impl Transform for (Qualified, TypeDecl) {
    type Out = ();

    fn transform<'a>(&self, context: &mut Context) -> Self::Out {
        // This block is used to classify the type definition. This is used later on to determine
        // how to lower the type. For example, if the type is a newtype, then we can just use the
        // underlying type.

        let classification = match &self.1 {
            TypeDecl::Abstract => TypeDef::Abstract,
            TypeDecl::Enum(constructors) => {
                if constructors.len() == 1 {
                    if constructors[0].1 == 1 {
                        context.add_constructor(constructors[0].0.clone(), ConsDef::NewType, 1);
                        TypeDef::NewType
                    } else if constructors[0].1 == 0 {
                        context.add_constructor(
                            constructors[0].0.clone(),
                            ConsDef::Enumerated(constructors[0].0.clone(), 0),
                            0,
                        );
                        TypeDef::Enumerated
                    } else {
                        context.add_constructor(
                            constructors[0].0.clone(),
                            ConsDef::Tuple,
                            constructors[0].1,
                        );
                        TypeDef::Tuple
                    }
                } else {
                    if constructors.iter().all(|x| x.1 == 0) {
                        for (id, (name, size)) in constructors.iter().enumerate() {
                            context.add_constructor(
                                name.clone(),
                                ConsDef::Enumerated(name.clone(), id),
                                *size,
                            );
                        }

                        TypeDef::Enumerated
                    } else {
                        for (id, (name, size)) in constructors.iter().enumerate() {
                            context.add_constructor(
                                name.clone(),
                                ConsDef::Heavy(name.clone(), id, *size),
                                *size,
                            );
                        }

                        TypeDef::Heavy
                    }
                }
            }
            TypeDecl::Record(fields) => {
                if fields.len() == 1 {
                    TypeDef::Heavy
                } else {
                    TypeDef::Record
                }
            }
        };

        context.types.insert(self.0.clone(), classification);
    }
}

impl Transform for Program<Type<Real>> {
    type Out = lambda::Program;

    fn transform<'a>(&self, context: &mut Context) -> Self::Out {
        let mut lets = vec![];
        let mut types = vec![];

        for (name, decl) in self.types.iter() {
            types.push((
                name.clone(),
                (name.clone(), decl.clone()).transform(context),
            ));
        }

        for (name, decl) in self.lets.iter() {
            lets.push((
                name.clone(),
                (name.clone(), decl.clone()).transform(context),
            ));
        }

        let definitions = context.constructors.borrow().clone();

        lambda::Program {
            lets,
            externals: self
                .externals
                .clone()
                .into_iter()
                .map(|(x, y)| (x, y.binding.clone()))
                .collect(),
            definitions,
            commands: self.commands.clone(),
        }
    }
}

pub struct Programs(pub Vec<Program<Type<Real>>>);

impl Transform for Programs {
    type Out = Vec<lambda::Program>;

    fn transform<'a>(&self, context: &mut Context) -> Self::Out {
        let mut programs: Vec<_> = vec![lambda::Program::default(); self.0.len()];

        // Multiple contexts to isolate the output of each program.
        let mut contexts = vec![Context::default(); self.0.len()];

        for (i, program) in self.0.iter().enumerate() {
            for (name, external) in &program.externals {
                programs[i]
                    .externals
                    .push((name.clone(), external.binding.clone()));
            }

            programs[i].commands.extend(program.commands.clone());

            for (name, type_expr) in &program.types {
                (name.clone(), type_expr.clone()).transform(&mut contexts[i]);
                let definitions = contexts[i].constructors.borrow().clone();
                programs[i].definitions = definitions;
            }

            for (name, (def, size)) in programs[i].definitions.clone() {
                context.add_constructor(name.clone(), def.clone(), size);

                let names: Vec<_> = (0..size)
                    .map(|_| context.new_var("v".to_string()))
                    .collect();

                programs[i]
                    .lets
                    .push((name.clone(), derive_let_from_constructor(name, names, def)))
            }
        }

        for (i, program) in self.0.iter().enumerate() {
            for (name, type_expr) in &program.lets {
                let let_expr = (name.clone(), type_expr.clone()).transform(context);
                programs[i].lets.push((name.clone(), let_expr));
            }
        }

        programs
    }
}

fn derive_let_from_constructor(
    name: Qualified,
    names: Vec<Symbol>,
    def: ConsDef,
) -> lambda::LetDecl {
    let body = match def {
        ConsDef::Enumerated(_, i) => Box::new(lambda::ExprKind::Literal(Box::new(
            LiteralKind::Integer(Symbol::intern(&i.to_string())),
        ))),
        ConsDef::Heavy(_, id, _) => Box::new(lambda::ExprKind::Object(
            id,
            names
                .iter()
                .map(|x| Box::new(lambda::ExprKind::Variable(x.clone())))
                .collect(),
        )),
        ConsDef::NewType => Box::new(lambda::ExprKind::Variable(names[0].clone())),
        ConsDef::Tuple => Box::new(lambda::ExprKind::Tuple(
            names
                .iter()
                .map(|x| Box::new(lambda::ExprKind::Variable(x.clone())))
                .collect(),
        )),
    };

    lambda::LetDecl {
        name: name.clone(),
        body: names.into_iter().rfold(body, |acc, name| {
            Box::new(lambda::ExprKind::Lambda(vec![name], acc))
        }),
        constants: None,
    }
}
