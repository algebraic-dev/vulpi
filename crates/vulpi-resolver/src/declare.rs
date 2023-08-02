//! This is the first phase of the resolution process. It takes a syntax tree and then checks for
//! each of the modules inside of it creating an ID for each of them. After that it returns a
//! [Solver] that is an abstraction for a next phase of the resolution process, the import
//! resolution phase that will solve all the imports and then return another [Solver] that is the
//! last one.

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::{Diagnostic, Report};

use vulpi_syntax::{concrete::tree::*, r#abstract::Qualified};

use crate::{
    error::{ResolverError, ResolverErrorKind},
    module_tree::ModuleTree,
    namespace::{self, Item, ModuleId, TypeValue, Value},
};

pub struct Context<'a> {
    pub reporter: Report,
    pub module_tree: &'a mut ModuleTree,
    pub namespaces: &'a mut Vec<namespace::Namespace>,
    pub name: Vec<Symbol>,
}

impl<'a> Context<'a> {
    pub fn new(
        reporter: Report,
        module_tree: &'a mut ModuleTree,
        namespaces: &'a mut Vec<namespace::Namespace>,
    ) -> Self {
        Self {
            reporter,
            module_tree,
            namespaces,
            name: Vec::new(),
        }
    }

    fn new_with_counter(
        reporter: Report,
        module_tree: &'a mut ModuleTree,
        namespaces: &'a mut Vec<namespace::Namespace>,
        name: Vec<Symbol>,
    ) -> Self {
        Self {
            reporter,
            module_tree,
            namespaces,
            name,
        }
    }

    pub fn report(&self, error: crate::error::ResolverError) {
        self.reporter.report(Diagnostic::new(error));
    }

    fn derive(&mut self, new_name: Symbol, pass_through: Option<ModuleId>) -> Context {
        let id = self.namespaces.len();
        self.derive_with_module_id(new_name, ModuleId(id), pass_through)
    }

    fn derive_with_module_id(
        &mut self,
        new_name: Symbol,
        id: ModuleId,
        pass_through: Option<ModuleId>,
    ) -> Context {
        let mut name = self.name.clone();
        name.push(new_name);

        self.module_tree.add(&name, id).unwrap();
        self.namespaces
            .push(namespace::Namespace::new(pass_through));

        Context::new_with_counter(
            self.reporter.clone(),
            self.module_tree,
            self.namespaces,
            name,
        )
    }

    fn genenerate_id(&mut self) -> ModuleId {
        ModuleId(self.namespaces.len())
    }

    pub fn qualified_name(&self, name: Symbol) -> Qualified {
        Qualified {
            path: self.module_tree.find(&self.name).unwrap().id.0,
            name,
        }
    }

    pub fn add_value(&mut self, span: Span, name: Symbol, value: Item<Value>) {
        let id = self.module_tree.find_mut(&self.name).unwrap().id;
        let old = self.namespaces[id.0].values.insert(name.clone(), value);

        if old.is_some() {
            self.report(ResolverError {
                span,
                kind: ResolverErrorKind::Redeclarated(name),
            })
        }
    }

    pub fn add_module(&mut self, span: Span, name: Symbol, value: Item<ModuleId>) {
        let id = self.module_tree.find_mut(&self.name).unwrap().id;
        let old = self.namespaces[id.0].modules.insert(name.clone(), value);

        if old.is_some() {
            self.report(ResolverError {
                span,
                kind: ResolverErrorKind::Redeclarated(name),
            })
        }
    }

    pub fn add_type(&mut self, span: Span, name: Symbol, value: Item<TypeValue>) {
        let id = self.module_tree.find_mut(&self.name).unwrap().id;
        let old = self.namespaces[id.0].types.insert(name.clone(), value);

        if old.is_some() {
            self.report(ResolverError {
                span,
                kind: ResolverErrorKind::Redeclarated(name),
            })
        }
    }

    fn merge(&mut self, namespace: namespace::Namespace) {
        for (key, value) in namespace.values {
            self.add_value(value.span.clone(), key, value);
        }

        for (key, value) in namespace.types {
            self.add_type(value.span.clone(), key, value);
        }

        for (key, value) in namespace.modules {
            self.add_module(value.span.clone(), key, value);
        }
    }

    pub fn current_id(&self) -> ModuleId {
        self.module_tree.find(&self.name).unwrap().id
    }
}

pub trait Declare {
    fn declare(&self, ctx: &mut Context);
}

impl From<Visibility> for namespace::Visibility {
    fn from(value: Visibility) -> Self {
        match value {
            Visibility::Public(_) => namespace::Visibility::Public,
            Visibility::Private => namespace::Visibility::Private,
        }
    }
}

impl Declare for EffectDecl {
    fn declare(&self, ctx: &mut Context) {
        let old = ctx.current_id();

        ctx.add_type(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: TypeValue::Effect(ctx.qualified_name(self.name.symbol())),
            },
        );

        let id = ctx.genenerate_id();

        ctx.add_module(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: id,
            },
        );

        let ctx = &mut ctx.derive_with_module_id(self.name.symbol(), id, Some(old));

        for field in &self.fields {
            ctx.add_value(
                field.0.name.0.value.span.clone(),
                field.0.name.symbol(),
                Item {
                    visibility: field.0.visibility.clone().into(),
                    span: field.0.name.0.value.span.clone(),
                    item: Value::Effect(ctx.qualified_name(field.0.name.symbol())),
                },
            );
        }
    }
}

impl Declare for ModuleDecl {
    fn declare(&self, old_ctx: &mut Context) {
        let id = old_ctx.genenerate_id();

        old_ctx.add_module(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: id,
            },
        );

        let ctx = &mut &mut old_ctx.derive_with_module_id(self.name.symbol(), id, None);

        if let Some(module) = &self.part {
            for top_level in &module.top_levels {
                top_level.declare(ctx);
            }
        }
    }
}

impl Declare for SumDecl {
    fn declare(&self, ctx: &mut Context) {
        for constructor in self.constructors.iter() {
            ctx.add_value(
                constructor.name.0.value.span.clone(),
                constructor.name.symbol(),
                Item {
                    visibility: namespace::Visibility::Public,
                    span: constructor.name.0.value.span.clone(),
                    item: Value::Constructor(ctx.qualified_name(constructor.name.symbol())),
                },
            );
        }
    }
}

impl Declare for RecordDecl {
    fn declare(&self, ctx: &mut Context) {
        for (field, _) in self.fields.iter() {
            ctx.add_value(
                field.name.0.value.span.clone(),
                field.name.symbol(),
                Item {
                    visibility: namespace::Visibility::Public,
                    span: field.name.0.value.span.clone(),
                    item: Value::Field(ctx.qualified_name(field.name.symbol())),
                },
            );
        }
    }
}

impl Declare for TypeDef {
    fn declare(&self, ctx: &mut Context) {
        match self {
            TypeDef::Sum(s) => s.declare(ctx),
            TypeDef::Record(s) => s.declare(ctx),
            TypeDef::Synonym(_) => (),
        }
    }
}

impl Declare for TypeDecl {
    fn declare(&self, ctx: &mut Context) {
        let old = ctx.current_id();

        ctx.add_type(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: match self.def {
                    Some((_, TypeDef::Sum(_))) => {
                        TypeValue::Enum(ctx.qualified_name(self.name.symbol()))
                    }
                    Some((_, TypeDef::Record(_))) => {
                        TypeValue::Record(ctx.qualified_name(self.name.symbol()))
                    }
                    Some((_, TypeDef::Synonym(_))) => {
                        todo!("Synonym types are not")
                    }
                    _ => TypeValue::Abstract(ctx.qualified_name(self.name.symbol())),
                },
            },
        );

        let id = ctx.genenerate_id();

        ctx.add_module(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: id,
            },
        );

        let ctx = &mut ctx.derive_with_module_id(self.name.symbol(), id, Some(old));

        if let Some(some) = &self.def {
            some.1.declare(ctx);
        }
    }
}

impl Declare for LetDecl {
    fn declare(&self, ctx: &mut Context) {
        ctx.add_value(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: Value::Function(ctx.qualified_name(self.name.symbol())),
            },
        );
    }
}

impl Declare for TopLevel {
    fn declare(&self, ctx: &mut Context) {
        match self {
            TopLevel::Use(_) => (),
            TopLevel::Type(typ) => {
                typ.declare(ctx);
            }
            TopLevel::Module(module) => {
                module.declare(ctx);
            }
            TopLevel::Effect(effect) => {
                effect.declare(ctx);
            }
            TopLevel::Let(decl) => {
                decl.declare(ctx);
            }
            TopLevel::Error(_) => (),
        }
    }
}

impl Declare for Program {
    fn declare(&self, ctx: &mut Context) {
        for top_level in &self.top_levels {
            top_level.declare(ctx);
        }
    }
}

pub trait ImportResolve {
    fn resolve_imports(&self, ctx: &mut Context);
}

impl ImportResolve for UseDecl {
    fn resolve_imports(&self, ctx: &mut Context) {
        let vec: Vec<_> = (&self.path).into();

        let Some(sub_tree) = ctx.module_tree.find(&ctx.name).and_then(|x| x.find(&vec)) else {
            return ctx.report(ResolverError {
                span: self.path.span.clone(),
                kind: ResolverErrorKind::NotFound(vec),
            });
        };

        if let Some(alias) = &self.alias {
            ctx.add_value(
                alias.alias.0.value.span.clone(),
                alias.alias.symbol(),
                Item {
                    visibility: namespace::Visibility::Public,
                    span: alias.alias.0.value.span.clone(),
                    item: Value::Module(sub_tree.id),
                },
            )
        } else {
            let namespace = ctx.namespaces[sub_tree.id.0].clone();
            ctx.merge(namespace);
        }
    }
}

impl ImportResolve for TopLevel {
    fn resolve_imports(&self, ctx: &mut Context) {
        if let TopLevel::Use(use_) = self {
            use_.resolve_imports(ctx);
        }
    }
}

impl ImportResolve for ModuleDecl {
    fn resolve_imports(&self, ctx: &mut Context) {
        let ctx = &mut ctx.derive(self.name.symbol(), None);

        if let Some(module) = &self.part {
            for top_level in module.modules() {
                top_level.resolve_imports(ctx);
            }

            for top_level in module.uses() {
                top_level.resolve_imports(ctx);
            }
        }
    }
}

impl ImportResolve for Program {
    fn resolve_imports(&self, ctx: &mut Context) {
        for top_level in self.modules() {
            top_level.resolve_imports(ctx);
        }

        for top_level in self.uses() {
            top_level.resolve_imports(ctx);
        }
    }
}
