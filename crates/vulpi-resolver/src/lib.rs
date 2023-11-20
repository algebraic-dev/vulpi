//! The resolver is responsible for taking a single concrete tree and turn it into an abstract
//! syntax tree with all the names resolved.

use std::cell::{Ref, RefMut};
use std::collections::HashMap;
use std::{cell::RefCell, rc::Rc};

use petgraph::prelude::DiGraph;
use petgraph::stable_graph::NodeIndex;

use vulpi_intern::Symbol;

use vulpi_location::{Span, Spanned};
use vulpi_report::{Diagnostic, Report};
use vulpi_syntax::concrete::tree::LetMode;
use vulpi_syntax::concrete::{self, tree};
use vulpi_syntax::r#abstract as abs;
use vulpi_syntax::r#abstract::Visibility;
use vulpi_vfs::path::{Path, Qualified};

pub mod cycle;
pub mod dependencies;
mod error;

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

pub struct Crate {
    pub name: Symbol,
}

/// Definition kind is the kind of a definition. It is used to store the definitions in the
/// namespace.
#[derive(Clone, Copy)]
pub enum DefinitionKind {
    Type,
    Value,
    Trait,
}

/// Definition bag is a bag of definitions. It is used to store the definitions of a module.
#[derive(Default, Clone)]
pub struct Bag<V> {
    pub types: V,
    pub values: V,
    pub traits: V,
}

impl<V> Bag<V> {
    pub fn apply<T>(&self, definition: DefinitionKind, f: impl FnOnce(&V) -> T) -> T {
        match definition {
            DefinitionKind::Type => f(&self.types),
            DefinitionKind::Value => f(&self.values),
            DefinitionKind::Trait => f(&self.values),
        }
    }
}

pub type Alias = (Qualified, abs::Visibility);

/// Namespace of a module.
pub struct Namespace {
    name: Path,
    declared: Bag<HashMap<Symbol, abs::Visibility>>,
    constants: HashMap<abs::Qualified, HashMap<abs::Qualified, Span>>,
    traits: HashMap<Symbol, HashMap<Symbol, Span>>,

    aliases: Bag<HashMap<Symbol, Alias>>,
    modules: HashMap<Symbol, (Path, abs::Visibility)>,
    submodules: HashMap<Symbol, Module>,
    available: HashMap<Path, Module>,
    opened: HashMap<Path, Visibility>,
}

pub fn from_upper_path(path: &concrete::Path<concrete::Upper>) -> Path {
    let mut path_result = Path { segments: vec![] };

    for segment in &path.segments {
        path_result.segments.push(segment.0.symbol());
    }

    path_result.segments.push(path.last.symbol());

    path_result
}

pub fn from_lower_path(path: &concrete::Path<concrete::Lower>) -> Qualified {
    let mut path_result = Path { segments: vec![] };

    for segment in &path.segments {
        path_result.segments.push(segment.0.symbol());
    }

    Qualified {
        path: path_result,
        name: path.last.symbol(),
    }
}

pub fn from_constructor_upper_path(path: &concrete::Path<concrete::Upper>) -> Qualified {
    let mut path_result = Path { segments: vec![] };

    for segment in &path.segments {
        path_result.segments.push(segment.0.symbol());
    }

    Qualified {
        path: path_result,
        name: path.last.symbol(),
    }
}

/// Module is a wrapper around the namespace. It is used to make the namespace mutable, and to
/// be easy to clone.
#[derive(Clone)]
pub struct Module(Rc<RefCell<Namespace>>);

/// Getters for the namespace.
impl Module {
    pub fn add_available(&self, path: Path, module: Module) {
        self.borrow_mut().available.insert(path, module);
    }

    pub fn modules_mut(&self) -> RefMut<'_, HashMap<Symbol, (Path, abs::Visibility)>> {
        std::cell::RefMut::map(self.borrow_mut(), |this| &mut this.modules)
    }

    pub fn modules(&self) -> Ref<'_, HashMap<Symbol, (Path, abs::Visibility)>> {
        std::cell::Ref::map(self.borrow(), |this| &this.modules)
    }

    pub fn name(&self) -> Ref<'_, Path> {
        std::cell::Ref::map(self.borrow(), |this| &this.name)
    }

    fn declared(&self) -> Ref<'_, Bag<HashMap<Symbol, abs::Visibility>>> {
        std::cell::Ref::map(self.borrow(), |this| &this.declared)
    }

    fn aliases(&self) -> Ref<'_, Bag<HashMap<Symbol, Alias>>> {
        std::cell::Ref::map(self.borrow(), |this| &this.aliases)
    }

    fn available(&self) -> Ref<'_, HashMap<Path, Module>> {
        std::cell::Ref::map(self.borrow(), |this| &this.available)
    }

    fn opened(&self) -> Ref<'_, HashMap<Path, abs::Visibility>> {
        std::cell::Ref::map(self.borrow(), |this| &this.opened)
    }

    fn traits(&self) -> RefMut<'_, HashMap<Symbol, HashMap<Symbol, Span>>> {
        std::cell::RefMut::map(self.borrow_mut(), |this| &mut this.traits)
    }

    fn opened_mut(&self) -> RefMut<'_, HashMap<Path, abs::Visibility>> {
        std::cell::RefMut::map(self.borrow_mut(), |this| &mut this.opened)
    }
}

/// Utility functions for the namespace.
impl Module {
    pub fn new(name: Path) -> Module {
        Module(Rc::new(RefCell::new(Namespace {
            name,
            declared: Default::default(),
            aliases: Default::default(),
            traits: Default::default(),
            constants: Default::default(),
            submodules: Default::default(),
            available: Default::default(),
            opened: Default::default(),
            modules: Default::default(),
        })))
    }

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
            DefinitionKind::Trait => bag.traits.insert(name, vis.into()),
        };
    }

    pub fn fork(&self, name: Symbol) -> Module {
        let path = { self.borrow().name.clone() };

        let module = self
            .borrow_mut()
            .submodules
            .entry(name.clone())
            .or_insert_with(|| Module::new(path.with(name.clone())))
            .clone();

        self.borrow_mut()
            .available
            .entry(path.with(name))
            .or_insert_with(|| module.clone())
            .clone()
    }
}

impl Module {
    fn search_declared(&self, kind: DefinitionKind, name: Symbol) -> Option<abs::Visibility> {
        self.declared()
            .apply(kind, |declared| declared.get(&name).cloned())
    }

    fn search_submodules(&self, name: Symbol) -> Option<Module> {
        self.borrow().submodules.get(&name).cloned()
    }

    fn search_aliases(&self, kind: DefinitionKind, name: Symbol) -> Option<Alias> {
        self.aliases()
            .apply(kind, |aliases| aliases.get(&name).cloned())
    }

    /// Search recursively for a definition in the module. It will return the path of the
    /// definition if it is found.
    ///
    /// The `last` parameter is the exclude list, it is used to avoid cyclic imports.
    fn search_recursively(
        &self,
        kind: DefinitionKind,
        name: Symbol,
        last: NodeIndex<u32>,
        nodes: &mut HashMap<Qualified, NodeIndex<u32>>,
        graph: &mut DiGraph<Qualified, ()>,
    ) -> Result<Option<Qualified>, Diagnostic> {
        let qualified = Qualified {
            path: self.name().clone(),
            name: name.clone(),
        };

        let actual = nodes
            .entry(qualified.clone())
            .or_insert_with(|| graph.add_node(qualified.clone()));

        graph.add_edge(last, *actual, ());

        if petgraph::algo::is_cyclic_undirected(&*graph) {
            return Ok(None);
        }

        if let Some(visibility) = self.search_declared(kind, name.clone()) {
            if let abs::Visibility::Private = visibility {
                return Err(Diagnostic::new(error::ResolverError {
                    span: Default::default(),
                    kind: error::ResolverErrorKind::PrivateDefinition,
                }));
            }

            return Ok(Some(qualified.clone()));
        }

        if let Some((qualified, visibility)) = self.search_aliases(kind, name.clone()) {
            if let abs::Visibility::Private = visibility {
                return Err(Diagnostic::new(error::ResolverError {
                    span: Default::default(),
                    kind: error::ResolverErrorKind::PrivateDefinition,
                }));
            }

            let available = self.available();
            let path = available.get(&qualified.path).cloned().ok_or_else(|| {
                Diagnostic::new(error::ResolverError {
                    span: Default::default(),
                    kind: error::ResolverErrorKind::InvalidPath(qualified.path.segments.clone()),
                })
            })?;

            return path.search_recursively(kind, qualified.name.clone(), last, nodes, graph);
        }

        for (path, visibility) in self.opened().iter() {
            let module = self.available().get(path).cloned();

            if module.is_none() || visibility == &abs::Visibility::Private {
                continue;
            }

            if let Some(path) = module.unwrap().search_recursively(
                kind,
                name.clone(),
                last,
                &mut nodes.clone(),
                &mut graph.clone(),
            )? {
                return Ok(Some(path));
            }
        }

        Ok(None)
    }

    pub fn search(
        &self,
        kind: DefinitionKind,
        name: Symbol,
    ) -> Result<Option<Qualified>, Diagnostic> {
        let mut graph = DiGraph::new();
        let mut map = HashMap::default();

        let qualified = Qualified {
            path: self.borrow().name.clone(),
            name: name.clone(),
        };

        let fst = graph.add_node(qualified.clone());
        map.insert(qualified.clone(), fst);

        if self.search_declared(kind, name.clone()).is_some() {
            return Ok(Some(qualified));
        }

        if let Some((qualified, _)) = self.search_aliases(kind, name.clone()) {
            let available = self.available();
            let path = available.get(&qualified.path).ok_or_else(|| {
                Diagnostic::new(error::ResolverError {
                    span: Default::default(),
                    kind: error::ResolverErrorKind::InvalidPath(qualified.path.segments.clone()),
                })
            })?;

            return path.search_recursively(kind, qualified.name, fst, &mut map, &mut graph);
        }

        for (path, _) in self.opened().iter() {
            let module = self.available().get(path).cloned();

            if module.is_none() {
                continue;
            }

            if let Some(path) = module.unwrap().search_recursively(
                kind,
                name.clone(),
                fst,
                &mut map.clone(),
                &mut graph.clone(),
            )? {
                return Ok(Some(path));
            }
        }

        Ok(None)
    }
}

/// The local context of the resolver. It contains the current module, the current scope, and the
/// report.
#[derive(Clone)]
pub struct Context {
    pub module: Module,
    scope: RefCell<Bag<im_rc::HashSet<Symbol>>>,
    reporter: Report,

    in_head: bool,
    constant: Option<abs::Qualified>,
}

impl Context {
    pub fn insert_constant(&mut self, path: abs::Qualified, span: Span) {
        if let Some(constant) = &self.constant {
            self.module
                .borrow_mut()
                .constants
                .entry(constant.clone())
                .or_default()
                .insert(path, span);
        }
    }

    pub fn set_constant(&mut self, constant: abs::Qualified) {
        self.constant = Some(constant);
        self.in_head = true;
    }

    pub fn reset_constant(&mut self) {
        self.constant = None;
        self.in_head = false;
    }

    pub fn new(name: Path, report: Report) -> Context {
        Context {
            module: Module::new(name),
            scope: Default::default(),
            reporter: report,

            in_head: false,
            constant: None,
        }
    }

    pub fn search(&self, kind: DefinitionKind, span: Span, name: Symbol) -> Option<abs::Qualified> {
        let searched = self.module.search(kind, name.clone());

        match searched {
            Ok(Some(res)) => Some(abs::Qualified {
                path: res.path.symbol(),
                name: res.name,
            }),
            Ok(None) => {
                self.reporter.report(Diagnostic::new(error::ResolverError {
                    span: span.clone(),
                    kind: error::ResolverErrorKind::NotFound(name),
                }));
                None
            }
            Err(err) => {
                self.reporter.report(err);
                None
            }
        }
    }

    pub fn get_path(
        &self,
        kind: DefinitionKind,
        span: Span,
        mut path: Qualified,
    ) -> Option<Qualified> {
        if let Some((alias, _)) = self.module.modules().get(&path.path.symbol()) {
            path.path = alias.clone();
        }

        let module = if path.path.is_empty() {
            self.module.clone()
        } else if let Some(module) = self.module.available().get(&path.path).cloned() {
            module
        } else if let Some(module) = self.module.search_submodules(path.path.symbol()) {
            module
        } else {
            self.reporter.report(Diagnostic::new(error::ResolverError {
                span: span.clone(),
                kind: error::ResolverErrorKind::InvalidPath(path.path.segments.clone()),
            }));
            return None;
        };

        let searched = module.search(kind, path.name.clone());

        match searched {
            Ok(Some(res)) => Some(res),
            Ok(None) => {
                self.reporter.report(Diagnostic::new(error::ResolverError {
                    span: span.clone(),
                    kind: error::ResolverErrorKind::NotFound(path.name),
                }));
                None
            }
            Err(err) => {
                self.reporter.report(err);
                None
            }
        }
    }

    pub fn resolve(
        &self,
        kind: DefinitionKind,
        span: Span,
        path: Qualified,
    ) -> Option<abs::Qualified> {
        let path = self.get_path(kind, span, path)?;
        Some(abs::Qualified {
            path: path.path.symbol(),
            name: path.name,
        })
    }

    /// Creates a nested context.
    pub fn fork(&self, name: Symbol) -> Context {
        let module = self.module.fork(name);
        let scope = Default::default();

        Context {
            module,
            scope,
            reporter: self.reporter.clone(),

            in_head: self.in_head,
            constant: self.constant.clone(),
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
            DefinitionKind::Trait => self.scope.borrow_mut().traits.insert(name),
        };
    }

    pub fn in_scope(&self, kind: DefinitionKind, name: Symbol) -> bool {
        let bag = &self.scope.borrow_mut();

        match kind {
            DefinitionKind::Type => bag.types.contains(&name),
            DefinitionKind::Value => bag.values.contains(&name),
            DefinitionKind::Trait => bag.traits.contains(&name),
        }
    }

    pub fn declared(&self, kind: DefinitionKind, name: Symbol) -> Option<abs::Visibility> {
        let bag = &self.module.borrow().declared;

        match kind {
            DefinitionKind::Type => bag.types.get(&name).cloned(),
            DefinitionKind::Value => bag.values.get(&name).cloned(),
            DefinitionKind::Trait => bag.traits.get(&name).cloned(),
        }
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

    use crate::error::ResolverError;

    use super::*;

    /// Resolve a top level declaration and returns the solver for it.
    pub fn resolve(ctx: Context, top_level: tree::TopLevel) -> Option<Solver<abs::TopLevel>> {
        use concrete::tree::TopLevel::*;

        match top_level {
            Let(let_decl) => Some(resolve_let(ctx, *let_decl, true).map(abs::TopLevel::Let)),
            Type(type_decl) => Some(resolve_type_decl(ctx, *type_decl).map(abs::TopLevel::Type)),
            Module(mod_decl) => Some(resolve_module(ctx, *mod_decl).map(abs::TopLevel::Module)),
            External(ext) => Some(resolve_external(ctx, *ext).map(abs::TopLevel::External)),
            Use(use_decl) => Some(resolve_use(ctx, *use_decl).map(|_| abs::TopLevel::Use)),
            Trait(trait_) => Some(resolve_trait(ctx, *trait_).map(abs::TopLevel::Trait)),
            Impl(impl_) => Some(resolve_impl(ctx, *impl_).map(abs::TopLevel::Impl)),
            Error(_) => None,
        }
    }

    pub fn resolve_trait(ctx: Context, decl: tree::TraitDecl) -> Solver<abs::TraitDecl> {
        let name = decl.name.symbol();
        let submodule = ctx.fork(decl.name.symbol());

        ctx.module
            .define(DefinitionKind::Type, decl.visibility.clone(), name.clone());

        ctx.module.traits().insert(
            name.clone(),
            decl.body
                .iter()
                .map(|x| (x.name.symbol(), x.name.0.value.span.clone()))
                .collect(),
        );

        let body = decl
            .body
            .into_iter()
            .map(|x| resolve_let_signature(submodule.clone(), x))
            .collect::<Vec<_>>();

        Solver::new(move |ctx| {
            ctx.scoped(|ctx| {
                let binders = decl
                    .binders
                    .into_iter()
                    .map(|x| transform_type_binder(ctx, x))
                    .collect::<Vec<_>>();

                let name = abs::Qualified {
                    path: ctx.module.name().symbol(),
                    name,
                };

                let supers = decl
                    .supers
                    .into_iter()
                    .map(|x| transform_type(ctx, *x.typ))
                    .collect::<Vec<_>>();

                let body = body.into_iter().map(|x| x.eval(ctx.clone())).collect();

                abs::TraitDecl {
                    name,
                    supers,
                    binders,
                    body,
                }
            })
        })
    }

    pub fn resolve_impl(ctx: Context, decl: tree::TraitImpl) -> Solver<Option<abs::TraitImpl>> {
        let let_names = decl
            .body
            .iter()
            .map(|x| {
                (
                    x.signature.name.symbol(),
                    x.signature.name.0.value.span.clone(),
                )
            })
            .collect::<HashMap<_, _>>();

        let body = decl
            .body
            .into_iter()
            .map(|x| resolve_let(ctx.clone(), x, false))
            .collect::<Vec<_>>();

        
        Solver::new(move |ctx| {
            let path = from_constructor_upper_path(&decl.name);
            let searched = ctx.get_path(DefinitionKind::Type, decl.name.span.clone(), path);

            ctx.scoped(|ctx| {
                let binders = decl
                    .types
                    .into_iter()
                    .map(|x| transform_type(ctx, *x))
                    .collect::<Vec<_>>();

                let body = body.into_iter().map(|x| x.eval(ctx.clone())).collect();

                if let Some(searched) = searched {
                    let module = ctx.module.available().get(&searched.path).cloned().unwrap();
                    let values = module.traits().get(&searched.name).cloned().unwrap();

                    let not_declared = let_names
                        .iter()
                        .filter(|x| !values.contains_key(x.0))
                        .collect::<Vec<_>>();

                    let over_declared = values
                        .iter()
                        .filter(|x| !let_names.contains_key(x.0))
                        .map(|(name, _)| (name.clone(), decl.name.span.clone()))
                        .collect::<Vec<_>>();

                    for (name, span) in over_declared {
                        ctx.reporter.report(Diagnostic::new(ResolverError {
                            span: span.clone(),
                            kind: error::ResolverErrorKind::NotFound(name.clone()),
                        }));
                    }

                    for (name, span) in not_declared {
                        ctx.reporter.report(Diagnostic::new(ResolverError {
                            span: span.clone(),
                            kind: error::ResolverErrorKind::NotImplemented(
                                searched.name.clone(),
                                name.clone(),
                            ),
                        }));
                    }

                    Some(abs::TraitImpl {
                        name: abs::Qualified {
                            path: searched.path.symbol(),
                            name: searched.name,
                        },
                        binders,
                        body,
                    })
                } else {
                    None
                }
            })
        })
    }

    pub fn resolve_let_signature(
        ctx: Context,
        sig: tree::LetSignature,
    ) -> Solver<abs::LetSignature> {
        let name = sig.name.symbol();

        // Gets the location of the name, so we can present the errors in a less annoying way
        // in the IDE.
        let span = sig.name.0.value.span.clone();

        ctx.module
            .define(DefinitionKind::Value, sig.visibility.clone(), name.clone());

        Solver::new(move |ctx| {
            ctx.scoped(|ctx| {
                let binders = sig
                    .binders
                    .into_iter()
                    .map(|x| transform_let_binder(ctx, x))
                    .collect();

                let name = abs::Qualified {
                    path: ctx.module.name().symbol(),
                    name,
                };

                abs::LetSignature {
                    span,
                    name,
                    visibility: sig.visibility.into(),
                    ret: sig
                        .ret
                        .map(|(_, type_kind)| transform_type(ctx, *type_kind)),
                    binders,
                }
            })
        })
    }

    /// Resolve a let declaration and returns the solver for it.
    pub fn resolve_let(ctx: Context, decl: tree::LetDecl, declare: bool) -> Solver<abs::LetDecl> {
        let name = decl.signature.name.symbol();

        // Gets the location of the name, so we can present the errors in a less annoying way
        // in the IDE.
        let span = decl.signature.name.0.value.span.clone();

        if declare {
            ctx.module.define(
                DefinitionKind::Value,
                decl.signature.visibility.clone(),
                name.clone(),
            );
        }

        Solver::new(move |ctx| {
            ctx.scoped(|ctx| {
                let binders = decl
                    .signature
                    .binders
                    .into_iter()
                    .map(|x| transform_let_binder(ctx, x))
                    .collect();

                let name = abs::Qualified {
                    path: ctx.module.name().symbol(),
                    name,
                };

                ctx.set_constant(name.clone());
                let body = pattern::transform_let_mode(ctx, decl.body);

                let constant = if let Some(name) = &ctx.constant {
                    Some(
                        ctx.module
                            .borrow_mut()
                            .constants
                            .entry(name.clone())
                            .or_default()
                            .clone(),
                    )
                } else {
                    ctx.module.borrow_mut().constants.remove(&name.clone());
                    None
                };

                ctx.reset_constant();

                let signature = abs::LetSignature {
                    span,
                    name,
                    visibility: decl.signature.visibility.into(),
                    ret: decl
                        .signature
                        .ret
                        .map(|(_, type_kind)| transform_type(ctx, *type_kind)),
                    binders,
                };

                abs::LetDecl {
                    signature,
                    body,
                    constant,
                }
            })
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
            Some((_, tree::TypeDef::Synonym(_synonym))) => todo!(),
        }

        let namespace = submodule.name().clone();

        Solver::new(move |ctx| {
            ctx.scoped(|ctx| {
                let binders = decl
                    .binders
                    .into_iter()
                    .map(|x| transform_type_binder(ctx, x))
                    .collect::<Vec<_>>();

                for binder in &binders {
                    match binder {
                        abs::TypeBinder::Implicit(x) => ctx.with(DefinitionKind::Type, x.clone()),
                        abs::TypeBinder::Explicit(x, _) => {
                            ctx.with(DefinitionKind::Type, x.clone())
                        }
                    }
                }

                let def = match decl.def {
                    None => abs::TypeDef::Abstract,
                    Some((_, tree::TypeDef::Record(record))) => {
                        let fields = record
                            .fields
                            .into_iter()
                            .map(|(field, _)| {
                                let symbol = field.name.symbol();
                                let transform_type = transform_type(ctx, *field.typ);
                                let into = field.visibility.into();
                                (
                                    abs::Qualified {
                                        path: namespace.clone().symbol(),
                                        name: symbol,
                                    },
                                    transform_type,
                                    into,
                                )
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
                                    .map(|x| transform_type(ctx, *x))
                                    .collect();
                                let typ = cons.typ.map(|x| transform_type(ctx, *x.1));
                                abs::Constructor {
                                    name: abs::Qualified {
                                        path: namespace.clone().symbol(),
                                        name,
                                    },
                                    args,
                                    typ,
                                }
                            })
                            .collect();

                        abs::TypeDef::Sum(abs::SumDecl { constructors })
                    }
                    Some((_, tree::TypeDef::Synonym(_synonym))) => todo!(),
                };

                abs::TypeDecl {
                    name: abs::Qualified {
                        path: ctx.module.name().symbol(),
                        name,
                    },
                    namespace: namespace.symbol(),
                    visibility: decl.visibility.into(),
                    binders,
                    def,
                }
            })
        })
    }

    /// Resolve an external declaration and returns the solver for it.
    pub fn resolve_external(ctx: Context, decl: tree::ExtDecl) -> Solver<abs::ExtDecl> {
        let name = decl.name.symbol();

        ctx.module
            .define(DefinitionKind::Value, decl.visibility.clone(), name.clone());

        let namespace = ctx.module.name().clone();

        Solver::new(move |module| abs::ExtDecl {
            name: abs::Qualified {
                path: namespace.clone().symbol(),
                name,
            },
            namespace: namespace.symbol(),
            visibility: decl.visibility.into(),
            typ: transform_type(&module, *decl.typ),
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
                        abs::TopLevel::Trait(t) => program.traits.push(t),
                        abs::TopLevel::Impl(Some(t)) => program.impls.push(t),
                        abs::TopLevel::Impl(None) => (),
                        abs::TopLevel::Use => (),
                    }
                }

                program
            })
        }

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

    pub fn resolve_use(ctx: Context, decl: tree::UseDecl) -> Solver<()> {
        if let Some(alias) = decl.alias {
            ctx.module.modules_mut().insert(
                alias.alias.symbol(),
                (from_upper_path(&decl.path), decl.visibility.clone().into()),
            );
        } else {
            ctx.module
                .opened_mut()
                .insert(from_upper_path(&decl.path), decl.visibility.clone().into());
        }

        Solver::new(move |ctx| {
            let path = from_upper_path(&decl.path);

            if !ctx.module.available().contains_key(&path) {
                ctx.reporter.report(Diagnostic::new(ResolverError {
                    span: decl.path.span.clone(),
                    kind: error::ResolverErrorKind::InvalidPath(path.segments),
                }));
            }
        })
    }
}

pub fn transform_literal(literal: tree::Literal) -> abs::Literal {
    let data = match literal.data {
        tree::LiteralKind::String(x) => abs::LiteralKind::String(x.symbol()),
        tree::LiteralKind::Char(x) => abs::LiteralKind::Char(x.symbol()),
        tree::LiteralKind::Integer(x) => abs::LiteralKind::Integer(x.symbol()),
        tree::LiteralKind::Float(x) => abs::LiteralKind::Float(x.symbol()),
        tree::LiteralKind::Unit(_) => abs::LiteralKind::Unit,
    };

    Box::new(Spanned {
        data,
        span: literal.span.clone(),
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
            tree::PatternKind::Constructor(x) => {
                let func = ctx.resolve(
                    DefinitionKind::Value,
                    pattern.span.clone(),
                    from_constructor_upper_path(&x),
                );
                match func {
                    Some(res) => abs::PatternKind::Application(abs::PatApplication {
                        func: res,
                        args: vec![],
                    }),
                    None => abs::PatternKind::Error,
                }
            }
            tree::PatternKind::Variable(x) => {
                if vars.contains(&x.symbol()) {
                    ctx.reporter.report(Diagnostic::new(error::ResolverError {
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
                let func = ctx.resolve(
                    DefinitionKind::Value,
                    pattern.span.clone(),
                    from_constructor_upper_path(&app.func),
                );

                let args = app
                    .args
                    .into_iter()
                    .map(|x| transform_pat(ctx, *x, vars))
                    .collect();

                match func {
                    Some(func) => abs::PatternKind::Application(abs::PatApplication { func, args }),
                    None => abs::PatternKind::Error,
                }
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
    pub fn transform_pattern_arm(ctx: &mut Context, arm: tree::PatternArm) -> abs::PatternArm {
        if !arm.patterns.is_empty() {
            ctx.reset_constant()
        }

        ctx.scoped(|ctx| abs::PatternArm {
            patterns: arm
                .patterns
                .into_iter()
                .map(|x| pattern::transform(ctx, *x.0))
                .collect(),
            expr: expr::transform(ctx, *arm.expr),
            guard: arm.guard.map(|x| expr::transform(ctx, *x.1)),
        })
    }

    /// Transform a let mode into a list of pattern arms.
    pub fn transform_let_mode(ctx: &mut Context, mode: LetMode) -> Vec<abs::PatternArm> {
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

    /// Transforms an expression into an abstract expression.
    pub fn transform(ctx: &mut Context, expr: concrete::tree::Expr) -> abs::Expr {
        use tree::ExprKind::*;

        let data = match expr.data {
            Lambda(lam) => {
                if ctx.in_head {
                    ctx.reset_constant()
                }

                return ctx.scoped(|ctx| {
                    let pats: Vec<_> = pattern::transform_row(ctx, lam.patterns);

                    let body = transform(ctx, *lam.expr);

                    pats.into_iter().rev().fold(body, |body, param| {
                        Box::new(Spanned {
                            data: abs::ExprKind::Lambda(abs::LambdaExpr { param, body }),
                            span: expr.span.clone(),
                        })
                    })
                });
            }
            Application(app) => {
                ctx.in_head = false;

                abs::ExprKind::Application(abs::ApplicationExpr {
                    app: abs::AppKind::Normal,
                    func: expr::transform(ctx, *app.func),
                    args: app
                        .args
                        .into_iter()
                        .map(|expr| transform(ctx, *expr))
                        .collect(),
                })
            }

            Variable(x) => {
                if ctx.in_scope(DefinitionKind::Value, x.symbol()) {
                    abs::ExprKind::Variable(x.symbol())
                } else {
                    let searched = ctx.search(DefinitionKind::Value, expr.span.clone(), x.symbol());

                    match searched {
                        Some(res) => {
                            ctx.insert_constant(res.clone(), expr.span.clone());
                            abs::ExprKind::Function(res)
                        }
                        None => abs::ExprKind::Error,
                    }
                }
            }
            Constructor(x) => {
                match ctx.resolve(
                    DefinitionKind::Value,
                    expr.span.clone(),
                    from_constructor_upper_path(&x),
                ) {
                    Some(res) => abs::ExprKind::Constructor(res),
                    None => abs::ExprKind::Error,
                }
            }
            Function(path) => {
                let qualified = from_lower_path(&path);
                let searched = ctx.resolve(DefinitionKind::Value, expr.span.clone(), qualified);
                match searched {
                    Some(res) => {
                        ctx.insert_constant(res.clone(), expr.span.clone());

                        abs::ExprKind::Function(res)
                    }
                    None => abs::ExprKind::Error,
                }
            }

            Projection(projection) => abs::ExprKind::Projection(abs::ProjectionExpr {
                expr: transform(ctx, *projection.expr),
                field: projection.field.symbol(),
            }),
            Binary(bin) => {
                ctx.in_head = false;

                let left = transform(ctx, *bin.left);
                let right = transform(ctx, *bin.right);

                let name = match bin.op {
                    tree::Operator::Add(_) => "add",
                    tree::Operator::Sub(_) => "sub",
                    tree::Operator::Mul(_) => "mul",
                    tree::Operator::Div(_) => "div",
                    tree::Operator::Rem(_) => "rem",
                    tree::Operator::And(_) => "and",
                    tree::Operator::Or(_) => "or",
                    tree::Operator::Xor(_) => "xor",
                    tree::Operator::Not(_) => "not",
                    tree::Operator::Eq(_) => "eq",
                    tree::Operator::Neq(_) => "neq",
                    tree::Operator::Lt(_) => "lt",
                    tree::Operator::Gt(_) => "gt",
                    tree::Operator::Le(_) => "le",
                    tree::Operator::Ge(_) => "ge",
                    tree::Operator::Shl(_) => "shl",
                    tree::Operator::Shr(_) => "shr",
                    tree::Operator::Pipe(_) => "pipe",
                    tree::Operator::Concat(_) => "concat",
                };

                let path = ctx.resolve(
                    DefinitionKind::Value,
                    expr.span.clone(),
                    Qualified {
                        path: Path {
                            segments: vec![Symbol::intern("Prelude")],
                        },
                        name: Symbol::intern(name),
                    },
                );

                if let Some(path) = path {
                    abs::ExprKind::Application(abs::ApplicationExpr {
                        app: abs::AppKind::Infix,
                        func: Box::new(Spanned::new(
                            abs::ExprKind::Function(path),
                            bin.op.get_span(),
                        )),
                        args: vec![left, right],
                    })
                } else {
                    abs::ExprKind::Error
                }
            }
            Let(let_expr) => {
                let body = expr::transform(ctx, *let_expr.body);
                ctx.scoped(|ctx| {
                    abs::ExprKind::Let(abs::LetExpr {
                        pattern: pattern::transform(ctx, *let_expr.pattern),
                        body,
                        value: expr::transform(ctx, *let_expr.value),
                    })
                })
            }
            When(when) => {
                ctx.in_head = false;
                abs::ExprKind::When(abs::WhenExpr {
                    scrutinee: when
                        .scrutinee
                        .into_iter()
                        .map(|(scrutinee, _)| transform(ctx, *scrutinee))
                        .collect(),
                    arms: when
                        .arms
                        .into_iter()
                        .map(|x| pattern::transform_pattern_arm(ctx, x))
                        .collect(),
                })
            }
            Do(do_expr) => ctx.scoped(|ctx| {
                abs::ExprKind::Do(abs::Block {
                    sttms: do_expr
                        .block
                        .statements
                        .into_iter()
                        .map(|x| transform_sttm(ctx, x))
                        .collect(),
                })
            }),
            Literal(x) => abs::ExprKind::Literal(transform_literal(x)),
            Annotation(x) => {
                let expr = transform(ctx, *x.expr);
                let ty = transform_type(ctx, *x.typ);

                abs::ExprKind::Annotation(abs::AnnotationExpr { expr, typ: ty })
            }
            RecordInstance(record_instance) => {
                ctx.in_head = false;
                let path = ctx.resolve(
                    DefinitionKind::Type,
                    expr.span.clone(),
                    from_constructor_upper_path(&record_instance.name),
                );

                match path {
                    Some(name) => abs::ExprKind::RecordInstance(abs::RecordInstance {
                        name,
                        fields: record_instance
                            .fields
                            .into_iter()
                            .map(|(field, _)| {
                                let name = field.name.symbol();
                                let expr = transform(ctx, *field.expr);
                                (field.name.0.value.span, name, expr)
                            })
                            .collect(),
                    }),
                    None => abs::ExprKind::Error,
                }
            }
            RecordUpdate(record_update) => {
                ctx.in_head = false;
                abs::ExprKind::RecordUpdate(abs::RecordUpdate {
                    expr: transform(ctx, *record_update.expr),
                    fields: record_update
                        .fields
                        .into_iter()
                        .map(|(field, _)| {
                            let name = field.name.symbol();
                            let expr = transform(ctx, *field.expr);
                            (field.name.0.value.span, name, expr)
                        })
                        .collect(),
                })
            }
            Tuple(tuple) => {
                ctx.in_head = false;
                abs::ExprKind::Tuple(abs::Tuple {
                    exprs: tuple
                        .data
                        .into_iter()
                        .map(|(item, _)| transform(ctx, *item))
                        .collect(),
                })
            }
            Parenthesis(parenthesis) => return transform(ctx, *parenthesis.data.0),
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

pub fn transform_type_binder(_ctx: &Context, binder: tree::TypeBinder) -> abs::TypeBinder {
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
        tree::TypeKind::Type(typ) => {
            let path = ctx.resolve(
                DefinitionKind::Type,
                concrete_type.span.clone(),
                from_constructor_upper_path(&typ),
            );
            match path {
                Some(res) => abs::TypeKind::Type(res),
                None => abs::TypeKind::Error,
            }
        }
        tree::TypeKind::TypeVariable(e) => abs::TypeKind::TypeVariable(e.symbol()),
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
        tree::TypeKind::Forall(forall) => ctx.scoped(|ctx| {
            let params = forall
                .params
                .into_iter()
                .map(|x| transform_type_binder(ctx, x))
                .collect::<Vec<_>>();

            for binder in &params {
                match binder {
                    abs::TypeBinder::Implicit(x) => ctx.with(DefinitionKind::Type, x.clone()),
                    abs::TypeBinder::Explicit(x, _) => ctx.with(DefinitionKind::Type, x.clone()),
                }
            }

            let body = transform_type(ctx, *forall.body);

            abs::TypeKind::Forall(abs::TypeForall { params, body })
        }),
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

    abs::Binder { pat, typ: ty }
}

pub fn transform_trait_binder(ctx: &Context, binder: tree::TraitBinder) -> abs::Type {
    transform_type(ctx, *binder.typ)
}

pub fn transform_let_binder(ctx: &Context, binder: tree::LetBinder) -> abs::LetBinder {
    match binder {
        tree::LetBinder::Param(t) => abs::LetBinder::Param(transform_binder(ctx,t)),
        tree::LetBinder::Trait(t) => abs::LetBinder::Trait(transform_trait_binder(ctx, t)),
    }
}

pub fn transform_sttm(ctx: &mut Context, sttm: concrete::tree::Sttm) -> abs::Sttm {
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

/// Resolve all the top level declarations of a program.
pub fn resolve(ctx: &Context, program: tree::Program) -> Solver<abs::Program> {
    let mut solvers = vec![];

    for top_level in program.top_levels {
        if let Some(res) = top_level::resolve(ctx.clone(), top_level) {
            solvers.push(res);
        }
    }

    Solver::new(|ctx| {
        let mut program = abs::Program::default();

        for solver in solvers {
            match solver.eval(ctx.clone()) {
                abs::TopLevel::Let(x) => program.lets.push(x),
                abs::TopLevel::Type(x) => program.types.push(x),
                abs::TopLevel::Module(x) => program.modules.push(x),
                abs::TopLevel::External(x) => program.externals.push(x),
                abs::TopLevel::Trait(x) => program.traits.push(x),
                abs::TopLevel::Impl(Some(t)) => program.impls.push(t),
                abs::TopLevel::Impl(None) => (),
                abs::TopLevel::Use => (),
            }
        }

        program
    })
}
