//! The resolver is responsible for taking a single concrete tree and turn it into an abstract
//! syntax tree with all the names resolved.

use std::cell::Ref;
use std::{cell::RefCell, rc::Rc};

use im_rc::HashMap;
use petgraph::data::Build;
use petgraph::prelude::{DiGraph, UnGraph};
use petgraph::stable_graph::NodeIndex;
use petgraph::{algo, visit};
use vulpi_intern::Symbol;

use vulpi_location::Spanned;
use vulpi_report::{Diagnostic, Report};
use vulpi_syntax::concrete::tree::LetMode;
use vulpi_syntax::concrete::{self, tree};
use vulpi_syntax::r#abstract as abs;
use vulpi_syntax::r#abstract::Visibility;
use vulpi_vfs::path::{Qualified, Path};

mod error;

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

pub struct Crate {
    pub name: Symbol,
}

#[derive(Clone, Copy)]
pub enum DefinitionKind {
    Type,
    Value,
    Module,
}

pub enum ScopeKind {
    Module,
    Local,
}

#[derive(Default, Clone)]
pub struct Bag<V> {
    pub types: V,
    pub values: V,
    pub modules: V,
}

impl<V> Bag<V> {
    pub fn apply<T>(&self, definition: DefinitionKind, f: impl FnOnce(&V) -> T) -> T {
        match definition {
            DefinitionKind::Type => f(&self.types),
            DefinitionKind::Value => f(&self.values),
            DefinitionKind::Module => f(&self.modules),
        }
    }
}

pub struct Namespace {
    name: Path,

    declared: Bag<HashMap<Symbol, abs::Visibility>>,
    aliases: Bag<HashMap<Symbol, (Qualified, abs::Visibility)>>,

    submodules: HashMap<Symbol, Module>,

    pub available: HashMap<Path, Module>,
    pub opened: Vec<Module>,
}

#[derive(Clone)]
pub struct Module(Rc<RefCell<Namespace>>);

impl Module {
    fn search_declared(&self, kind: DefinitionKind, name: Symbol) -> Option<abs::Visibility> {
        self.borrow()
            .declared
            .apply(kind, |declared| declared.get(&name).cloned())
    }

    fn search_aliases(
        &self,
        kind: DefinitionKind,
        name: Symbol,
    ) -> Option<(Qualified, abs::Visibility)> {
        self.borrow()
            .aliases
            .apply(kind, |aliases| aliases.get(&name).cloned())
    }

    fn search_recursively(
        &self,
        kind: DefinitionKind,
        name: Symbol,
        last: NodeIndex<u32>,
        breadcrumbs: &mut DiGraph<Qualified, ()>,
    ) -> Result<Option<Path>, Diagnostic> {
        let actual = breadcrumbs.add_node(Qualified {
            path: self.borrow().name.clone(),
            name: name.clone(),
        });

        breadcrumbs.add_edge(last, actual, ());

        if petgraph::algo::is_cyclic_undirected(&*breadcrumbs) {
            return Err(Diagnostic::new(error::ResolverError {
                span: Default::default(),
                kind: error::ResolverErrorKind::CyclicImport,
            }));
        }

        if let Some(visibility) = self.search_declared(kind, name.clone()) {
            if let abs::Visibility::Private = visibility {
                return Err(Diagnostic::new(error::ResolverError {
                    span: Default::default(),
                    kind: error::ResolverErrorKind::PrivateDefinition,
                }));
            }

            return Ok(Some(self.borrow().name.clone()));
        }

        if let Some((qualified, visibility)) = self.search_aliases(kind, name.clone()) {
            if let abs::Visibility::Private = visibility {
                return Err(Diagnostic::new(error::ResolverError {
                    span: Default::default(),
                    kind: error::ResolverErrorKind::PrivateDefinition,
                }));
            }

            let path = if let Some(path) = { self.borrow().available.get(&qualified.path) } {
                path.clone()
            } else {
                return Err(Diagnostic::new(error::ResolverError {
                    span: Default::default(),
                    kind: error::ResolverErrorKind::InvalidPath(qualified.path.segments.clone()),
                }));
            };

            return path.search_recursively(kind, qualified.name.clone(), last, breadcrumbs);
        }

        for module in &self.borrow().opened {
            if let Some(path) = module.search_recursively(kind, name.clone(), last, breadcrumbs)? {
                return Ok(Some(path));
            }
        }

        Ok(None)
    }

    pub fn search(&self, kind: DefinitionKind, name: Symbol) -> Result<Option<Path>, Diagnostic> {
        let mut graph = DiGraph::new();
        
        let fst = graph.add_node(Qualified {
            path: self.borrow().name.clone(),
            name: name.clone(),
        });

        if self.search_declared(kind, name.clone()).is_some() {
            return Ok(Some(self.borrow().name.clone()));
        }

        if let Some((qualified, _)) = self.search_aliases(kind, name.clone()) {
            let borrow = self.borrow();
            
            let path = if let Some(path) = borrow.available.get(&qualified.path) {
                path
            } else {
                return Err(Diagnostic::new(error::ResolverError {
                    span: Default::default(),
                    kind: error::ResolverErrorKind::InvalidPath(qualified.path.segments.clone()),
                }));
            };

            return path.search_recursively(kind, qualified.name, fst, &mut graph);
        }

        for module in &self.borrow().opened {
            if let Some(path) = module.search_recursively(kind, name.clone(), fst, &mut graph)? {
                return Ok(Some(path));
            }
        }

        Ok(None)
    }
}
#[derive(Clone)]
pub struct Context {
    module: Module,
    scope: RefCell<Bag<im_rc::HashSet<Symbol>>>,
    report: Report,
}

impl Context {
    pub fn fork(&self, name: Symbol) -> Context {
        let module = self.module.fork(name);
        let scope = Default::default();

        Context {
            module,
            scope,
            report: self.report.clone(),
        }
    }

    pub fn scoped<T>(&self, fun: impl FnOnce(&mut Context) -> T) -> T {
        let mut ctx = self.clone();
        fun(&mut ctx)
    }

    pub fn with(&self, kind: DefinitionKind, name: Symbol) {
        match kind {
            DefinitionKind::Type => self.scope.borrow_mut().types.insert(name),
            DefinitionKind::Value => self.scope.borrow_mut().values.insert(name),
            DefinitionKind::Module => self.scope.borrow_mut().modules.insert(name),
        };
    }

    pub fn declared(&self, kind: DefinitionKind, name: Symbol) -> Option<abs::Visibility> {
        let bag = &self.module.borrow().declared;

        match kind {
            DefinitionKind::Type => bag.types.get(&name).cloned(),
            DefinitionKind::Value => bag.values.get(&name).cloned(),
            DefinitionKind::Module => bag.modules.get(&name).cloned(),
        }
    }
}

impl Module {
    pub fn new(name: Path) -> Module {
        Module(Rc::new(RefCell::new(Namespace {
            name,
            declared: Default::default(),
            aliases: Default::default(),
            submodules: Default::default(),
            available: Default::default(),
            opened: Default::default(),
        })))
    }
}

impl Module {
    pub fn borrow(&self) -> Ref<Namespace> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> std::cell::RefMut<Namespace> {
        self.0.borrow_mut()
    }

    /// Defines a name in the current namespace. It takes the visibility of the definition, the
    /// kind of the definition, and the name of the definition.
    pub fn define<Vis: Into<abs::Visibility>>(&self, kind: DefinitionKind, vis: Vis, name: Symbol) {
        let bag = &mut self.borrow_mut().declared;

        match kind {
            DefinitionKind::Type => bag.types.insert(name, vis.into()),
            DefinitionKind::Value => bag.values.insert(name, vis.into()),
            DefinitionKind::Module => bag.modules.insert(name, vis.into()),
        };
    }

    pub fn fork(&self, name: Symbol) -> Module {
        let path = { self.borrow().name.clone() };

        self.borrow_mut()
            .submodules
            .entry(name.clone())
            .or_insert_with(|| Module::new(path.with(name)))
            .clone()
    }
}

pub struct Solver<T> {
    run: Box<dyn FnOnce(Context) -> T>,
}

impl<T: 'static> Solver<T> {
    pub fn new(run: impl FnOnce(Context) -> T + 'static) -> Self {
        Self { run: Box::new(run) }
    }

    pub fn map<U: 'static>(self, f: impl FnOnce(T) -> U + 'static) -> Solver<U> {
        Solver::new(move |namespace| f((self.run)(namespace)))
    }

    pub fn eval(self, namespace: Context) -> T {
        (self.run)(namespace)
    }
}

/// Top level declarations are the ones that can be declared at the top level of a module.
pub mod top_level {
    use super::*;

    /// Resolve a top level declaration and returns the solver for it.
    pub fn resolve(ctx: Context, top_level: tree::TopLevel) -> Option<Solver<abs::TopLevel>> {
        use concrete::tree::TopLevel::*;

        match top_level {
            Let(let_decl) => Some(resolve_let(ctx, *let_decl).map(abs::TopLevel::Let)),
            Type(type_decl) => Some(resolve_type_decl(ctx, *type_decl).map(abs::TopLevel::Type)),
            Module(mod_decl) => Some(resolve_module(ctx, *mod_decl).map(abs::TopLevel::Module)),
            External(ext) => Some(resolve_external(ctx, *ext).map(abs::TopLevel::External)),
            Use(use_decl) => {
                declare_use(ctx, *use_decl);
                None
            }
            Error(_) => None,
        }
    }

    /// Resolve a let declaration and returns the solver for it.
    pub fn resolve_let(ctx: Context, decl: tree::LetDecl) -> Solver<abs::LetDecl> {
        let name = decl.name.symbol();

        // Gets the location of the name, so we can present the errors in a less annoying way
        // in the IDE.
        let span = decl.name.0.value.span.clone();

        ctx.module
            .define(DefinitionKind::Value, decl.visibility.clone(), name.clone());

        Solver::new(move |module| abs::LetDecl {
            span,
            name,
            visibility: decl.visibility.into(),
            body: pattern::transform_let_mode(&module, decl.body),
            ret: decl
                .ret
                .map(|(_, type_kind)| transform_type(&module, *type_kind)),
            binders: decl
                .binders
                .into_iter()
                .map(|x| transform_binder(&module, x))
                .collect(),
        })
    }

    /// Resolve a type declaration and returns the solver for it.
    pub fn resolve_type_decl(ctx: Context, decl: tree::TypeDecl) -> Solver<abs::TypeDecl> {
        let name = decl.name.symbol();
        let submodule = ctx.module.fork(decl.name.symbol());

        ctx.module
            .define(DefinitionKind::Type, decl.visibility.clone(), name.clone());

        match &decl.def {
            None => {}
            Some((_, tree::TypeDef::Record(record))) => {
                for (field, _) in &record.fields {
                    let name = field.name.symbol();
                    let vis = into_field_visiblity(field.visibility.clone().into());
                    submodule.define(DefinitionKind::Value, vis, name);
                }
            }
            Some((_, tree::TypeDef::Sum(sum))) => {
                for cons in &sum.constructors {
                    let name = cons.name.symbol();
                    submodule.define(DefinitionKind::Value, Visibility::Public, name);
                }
            }
            Some((_, tree::TypeDef::Synonym(synonym))) => todo!(),
        }

        Solver::new(move |module| {
            let def = match decl.def {
                None => abs::TypeDef::Abstract,
                Some((_, tree::TypeDef::Record(record))) => {
                    let fields = record
                        .fields
                        .into_iter()
                        .map(|(field, _)| {
                            let symbol = field.name.symbol();
                            let transform_type = transform_type(&module, *field.ty);
                            let into = field.visibility.into();
                            (symbol, transform_type, into)
                        })
                        .collect();

                    abs::TypeDef::Record(abs::RecordDecl { fields })
                }
                Some((_, tree::TypeDef::Sum(sum))) => {
                    let constructors = sum
                        .constructors
                        .into_iter()
                        .map(|cons| {
                            let name = cons.name.symbol();
                            let args = cons
                                .args
                                .into_iter()
                                .map(|x| transform_type(&module, *x))
                                .collect();
                            let typ = cons.typ.map(|x| transform_type(&module, *x.1));
                            abs::Constructor { name, args, typ }
                        })
                        .collect();

                    abs::TypeDef::Sum(abs::SumDecl { constructors })
                }
                Some((_, tree::TypeDef::Synonym(synonym))) => todo!(),
            };

            abs::TypeDecl {
                name,
                visibility: decl.visibility.into(),
                binders: decl
                    .binders
                    .into_iter()
                    .map(|x| transform_type_binder(&module, x))
                    .collect(),
                def,
            }
        })
    }

    /// Resolve an external declaration and returns the solver for it.
    pub fn resolve_external(ctx: Context, decl: tree::ExtDecl) -> Solver<abs::ExtDecl> {
        let name = decl.name.symbol();

        ctx.module
            .define(DefinitionKind::Value, decl.visibility.clone(), name.clone());

        Solver::new(move |module| abs::ExtDecl {
            name,
            visibility: decl.visibility.into(),
            ty: transform_type(&module, *decl.typ),
            ret: decl.str.symbol(),
        })
    }

    /// Resolve a module declaration and returns the solver for it.
    pub fn resolve_module(ctx: Context, decl: tree::ModuleDecl) -> Solver<abs::ModuleDecl> {
        pub fn resolve_module_inline(
            ctx: Context,
            decl: tree::ModuleInline,
        ) -> Solver<abs::Program> {
            let mut solvers = vec![];

            for top_level in decl.top_levels {
                if let Some(solver) = resolve(ctx.clone(), top_level) {
                    solvers.push(solver);
                }
            }

            Solver::new(move |ctx| {
                let mut program = abs::Program::default();

                for solver in solvers {
                    match solver.eval(ctx.clone()) {
                        abs::TopLevel::Let(x) => program.lets.push(x),
                        abs::TopLevel::Type(x) => program.types.push(x),
                        abs::TopLevel::Module(x) => program.modules.push(x),
                        abs::TopLevel::External(x) => program.externals.push(x),
                    }
                }

                program
            })
        }

        ctx.module.define(
            DefinitionKind::Module,
            decl.visibility.clone(),
            decl.name.symbol(),
        );

        let new_context = ctx.fork(decl.name.symbol());
        let solver = decl
            .part
            .map(|x| resolve_module_inline(new_context.clone(), x));

        Solver::new(move |ctx| abs::ModuleDecl {
            visibility: decl.visibility.into(),
            name: decl.name.symbol(),
            decls: solver.map(|x| x.eval(ctx)),
        })
    }

    pub fn declare_use(module: Context, _decl: tree::UseDecl) {}
}

pub fn transform_literal(lit: tree::Literal) -> abs::Literal {
    let data = match lit.data {
        tree::LiteralKind::String(x) => abs::LiteralKind::String(x.symbol()),
        tree::LiteralKind::Char(x) => abs::LiteralKind::Char(x.symbol()),
        tree::LiteralKind::Integer(x) => abs::LiteralKind::Integer(x.symbol()),
        tree::LiteralKind::Float(x) => abs::LiteralKind::Float(x.symbol()),
        tree::LiteralKind::Unit(_) => abs::LiteralKind::Unit,
    };

    Box::new(Spanned {
        data,
        span: lit.span.clone(),
    })
}

/// Patterns are the ones that can be used in a match expression.
pub mod pattern {
    use im_rc::HashSet;
    use vulpi_report::Diagnostic;

    use super::*;

    fn transform_pat(
        ctx: &Context,
        pattern: tree::Pattern,
        vars: &mut HashSet<Symbol>,
    ) -> abs::Pattern {
        let data = match pattern.data {
            tree::PatternKind::Wildcard(_) => abs::PatternKind::Wildcard,
            tree::PatternKind::Constructor(_) => todo!(),
            tree::PatternKind::Variable(x) => {
                if vars.contains(&x.symbol()) {
                    ctx.report.report(Diagnostic::new(error::ResolverError {
                        span: pattern.span.clone(),
                        kind: error::ResolverErrorKind::DuplicatePattern(x.symbol()),
                    }));
                    abs::PatternKind::Error
                } else {
                    vars.insert(x.symbol());
                    abs::PatternKind::Variable(x.symbol())
                }
            }
            tree::PatternKind::Literal(x) => {
                let lit = transform_literal(x);
                abs::PatternKind::Literal(lit)
            }
            tree::PatternKind::Annotation(app) => {
                let pat = transform_pat(ctx, *app.left, vars);
                let typ = transform_type(ctx, *app.right);

                abs::PatternKind::Ascription(abs::PatAscription { pat, typ })
            }
            tree::PatternKind::Tuple(tuple) => {
                let tuple = tuple
                    .into_iter()
                    .map(|x| transform_pat(ctx, x.0, vars))
                    .collect();

                abs::PatternKind::Tuple(tuple)
            }
            tree::PatternKind::Application(app) => {
                let func = todo!();

                let args = app
                    .args
                    .into_iter()
                    .map(|x| transform_pat(ctx, *x, vars))
                    .collect();

                abs::PatternKind::Application(abs::PatApplication { func, args })
            }
            tree::PatternKind::Parenthesis(x) => {
                return transform_pat(ctx, *x.data, vars);
            }
        };

        Box::new(Spanned {
            data,
            span: pattern.span.clone(),
        })
    }

    /// Transform a pattern into an abstract pattern.
    pub fn transform(ctx: &Context, pattern: tree::Pattern) -> abs::Pattern {
        let mut vars = Default::default();

        let pattern = transform_pat(ctx, pattern, &mut vars);

        for var in vars {
            ctx.with(DefinitionKind::Value, var);
        }

        pattern
    }

    pub fn transform_row(ctx: &Context, patterns: Vec<Box<tree::Pattern>>) -> Vec<abs::Pattern> {
        let mut vars = Default::default();

        let patterns = patterns
            .into_iter()
            .map(|x| transform_pat(ctx, *x, &mut vars))
            .collect::<Vec<_>>();

        for var in vars {
            ctx.with(DefinitionKind::Value, var);
        }

        patterns
    }

    /// Transform a pattern into an abstract pattern.
    pub fn transform_pattern_arm(ctx: &Context, arm: tree::PatternArm) -> abs::PatternArm {
        abs::PatternArm {
            patterns: arm
                .patterns
                .into_iter()
                .map(|x| pattern::transform(ctx, *x.0))
                .collect(),
            expr: expr::transform(ctx, *arm.expr),
            guard: arm.guard.map(|x| expr::transform(ctx, *x.1)),
        }
    }

    /// Transform a let mode into a list of pattern arms.
    pub fn transform_let_mode(ctx: &Context, mode: LetMode) -> Vec<abs::PatternArm> {
        match mode {
            LetMode::Body(_, expr) => {
                vec![abs::PatternArm {
                    patterns: vec![],
                    expr: expr::transform(ctx, *expr),
                    guard: None,
                }]
            }
            LetMode::Cases(cases) => cases
                .into_iter()
                .map(|x| transform_pattern_arm(ctx, x.arm))
                .collect(),
        }
    }
}

/// Expressions are the ones that can be used in a function body.
pub mod expr {
    use super::*;

    pub fn transform(ctx: &Context, expr: concrete::tree::Expr) -> abs::Expr {
        let data = match expr.data {
            tree::ExprKind::Lambda(lam) => {
                let pats: Vec<_> = pattern::transform_row(ctx, lam.patterns);

                let body = transform(ctx, *lam.expr);

                return pats.into_iter().rev().fold(body, |body, param| {
                    Box::new(Spanned {
                        data: abs::ExprKind::Lambda(abs::LambdaExpr { param, body }),
                        span: expr.span.clone(),
                    })
                });
            }
            tree::ExprKind::Application(app) => {
                let func = transform(ctx, *app.func);
                let args = app.args.into_iter().map(|x| transform(ctx, *x)).collect();

                abs::ExprKind::Application(abs::ApplicationExpr {
                    app: abs::AppKind::Normal,
                    func,
                    args,
                })
            }

            tree::ExprKind::Variable(_) => todo!(),
            tree::ExprKind::Constructor(_) => todo!(),
            tree::ExprKind::Function(_) => todo!(),

            tree::ExprKind::Projection(proj) => {
                let expr = transform(ctx, *proj.expr);
                let field = proj.field.symbol();

                abs::ExprKind::Projection(abs::ProjectionExpr { expr, field })
            }
            tree::ExprKind::Binary(_) => todo!(),
            tree::ExprKind::Let(let_) => {
                let pattern = pattern::transform(ctx, *let_.pattern);
                let body = transform(ctx, *let_.body);
                let value = transform(ctx, *let_.value);

                abs::ExprKind::Let(abs::LetExpr {
                    pattern,
                    body,
                    value,
                })
            }
            tree::ExprKind::When(when) => {
                let arms = when
                    .arms
                    .into_iter()
                    .map(|x| pattern::transform_pattern_arm(ctx, x))
                    .collect();

                let scrutinee = when
                    .scrutinee
                    .into_iter()
                    .map(|x| transform(ctx, *x.0))
                    .collect();

                abs::ExprKind::When(abs::WhenExpr { scrutinee, arms })
            }
            tree::ExprKind::Do(do_) => {
                let sttms = do_
                    .block
                    .statements
                    .into_iter()
                    .map(|x| transform_sttm(ctx, x))
                    .collect();

                abs::ExprKind::Do(abs::Block { sttms })
            }
            tree::ExprKind::Literal(x) => abs::ExprKind::Literal(transform_literal(x)),
            tree::ExprKind::Annotation(x) => {
                let expr = transform(ctx, *x.expr);
                let ty = transform_type(ctx, *x.ty);

                abs::ExprKind::Annotation(abs::AnnotationExpr { expr, ty })
            }
            tree::ExprKind::RecordInstance(u) => {
                let fields = u
                    .fields
                    .into_iter()
                    .map(|(field, _)| {
                        let name = field.name.symbol();
                        let expr = transform(ctx, *field.expr);
                        (field.name.0.value.span, name, expr)
                    })
                    .collect();

                abs::ExprKind::RecordInstance(abs::RecordInstance {
                    name: todo!(),
                    fields,
                })
            }
            tree::ExprKind::RecordUpdate(u) => {
                let expr = transform(ctx, *u.expr);

                let fields = u
                    .fields
                    .into_iter()
                    .map(|(field, _)| {
                        let name = field.name.symbol();
                        let expr = transform(ctx, *field.expr);
                        (field.name.0.value.span, name, expr)
                    })
                    .collect();

                abs::ExprKind::RecordUpdate(abs::RecordUpdate { expr, fields })
            }
            tree::ExprKind::Tuple(x) => {
                let exprs = x.data.into_iter().map(|x| transform(ctx, *x.0)).collect();

                abs::ExprKind::Tuple(abs::Tuple { exprs })
            }
            tree::ExprKind::Parenthesis(x) => return transform(ctx, *x.data.0),
        };

        Box::new(Spanned {
            data,
            span: expr.span.clone(),
        })
    }
}

/// The super module can access all the names in the module of an struct, so this is useful
/// to avoid having to pass the module around.
pub fn into_field_visiblity(vis: abs::Visibility) -> abs::Visibility {
    match vis {
        abs::Visibility::Public => abs::Visibility::Public,
        abs::Visibility::Private => abs::Visibility::Super,
        abs::Visibility::Super => abs::Visibility::Super,
    }
}

pub fn transform_kind(kind: tree::Kind) -> abs::Kind {
    let data = match kind.data {
        tree::KindType::Star(_) => abs::KindType::Star,
        tree::KindType::Variable(x) => match x.symbol().get().as_str() {
            "Type" => abs::KindType::Star,
            "Constraint" => abs::KindType::Constraint,
            _ => todo!("add error that the kind is not recognized"),
        },
        tree::KindType::Arrow(x, _, y) => {
            abs::KindType::Arrow(transform_kind(*x), transform_kind(*y))
        }
        tree::KindType::Parenthesis(x) => return transform_kind(*x.data),
    };

    Box::new(Spanned {
        data,
        span: kind.span.clone(),
    })
}

pub fn transform_type_binder(ctx: &Context, binder: tree::TypeBinder) -> abs::TypeBinder {
    match binder {
        tree::TypeBinder::Implicit(x) => abs::TypeBinder::Implicit(x.symbol()),
        tree::TypeBinder::Explicit(t) => {
            abs::TypeBinder::Explicit(t.data.name.symbol(), transform_kind(*t.data.kind))
        }
    }
}

pub fn transform_type(ctx: &Context, concrete_type: tree::Type) -> abs::Type {
    let data = match concrete_type.data {
        tree::TypeKind::Parenthesis(x) => return transform_type(ctx, *x.data.0),
        tree::TypeKind::Tuple(x) => abs::TypeKind::Tuple(
            x.data
                .into_iter()
                .map(|x| transform_type(ctx, *x.0))
                .collect(),
        ),
        tree::TypeKind::Type(_) => todo!(),
        tree::TypeKind::TypeVariable(_) => todo!(),
        tree::TypeKind::Arrow(x) => abs::TypeKind::Arrow(abs::PiType {
            left: transform_type(ctx, *x.left),
            right: transform_type(ctx, *x.right),
        }),
        tree::TypeKind::Application(app) => {
            let func = transform_type(ctx, *app.func);
            let args = app
                .args
                .into_iter()
                .map(|x| transform_type(ctx, *x))
                .collect();

            abs::TypeKind::Application(abs::TypeApplication { func, args })
        }
        tree::TypeKind::Forall(forall) => {
            let params = forall
                .params
                .into_iter()
                .map(|x| transform_type_binder(ctx, x))
                .collect();

            let body = transform_type(ctx, *forall.body);

            abs::TypeKind::Forall(abs::TypeForall { params, body })
        }
        tree::TypeKind::Unit(_) => abs::TypeKind::Unit,
    };

    Box::new(Spanned {
        data,
        span: concrete_type.span.clone(),
    })
}

pub fn transform_binder(ctx: &Context, binder: tree::Binder) -> abs::Binder {
    let pat = pattern::transform(ctx, *binder.pattern);
    let ty = transform_type(ctx, *binder.typ);

    abs::Binder { pat, ty }
}

pub fn transform_sttm(ctx: &Context, sttm: concrete::tree::Sttm) -> abs::Sttm {
    let data = match sttm.data {
        tree::StatementKind::Let(let_sttm) => {
            let pat = pattern::transform(ctx, *let_sttm.pattern);
            let expr = expr::transform(ctx, *let_sttm.expr);

            abs::SttmKind::Let(abs::LetSttm { pat, expr })
        }
        tree::StatementKind::Expr(expr) => {
            let expr = expr::transform(ctx, *expr);
            abs::SttmKind::Expr(expr)
        }
        tree::StatementKind::Error(_) => abs::SttmKind::Error,
    };

    abs::Sttm {
        data,
        span: sttm.span.clone(),
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_petgraph_acyclic() {}
}
