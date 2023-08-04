//! This is the first phase of the resolution process. It takes a syntax tree and then checks for
//! each of the modules inside of it creating an ID for each of them.

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::{Diagnostic, Report};
use vulpi_syntax::concrete::top_level::Visibility;
use vulpi_syntax::concrete::tree::*;

use crate::namespace;
use crate::{
    error::{ResolverError, ResolverErrorKind},
    namespace::{Item, Namespace, Namespaces, Resolve, TypeValue, Value},
    paths,
};
use vulpi_syntax::r#abstract::Qualified;

pub struct Context<'a> {
    pub reporter: Report,
    pub namespaces: &'a mut Namespaces,
    pub path: paths::Path,
}

impl<'a> Context<'a> {
    pub fn scope<T>(&mut self, name: Symbol, function: impl FnOnce(&mut Context) -> T) -> T {
        self.path.push(name);
        self.namespaces.add(self.path);
        let res = function(self);
        self.path.pop();
        res
    }

    pub fn current(&self) -> Symbol {
        self.path.symbol()
    }

    pub fn report(&self, error: crate::error::ResolverError) {
        self.reporter.report(Diagnostic::new(error));
    }

    fn report_redeclareted(&mut self, span: Span, name: Symbol) {
        self.report(ResolverError {
            span,
            kind: ResolverErrorKind::Redeclarated(name),
        })
    }

    fn report_not_found(&mut self, name: paths::Path) {
        self.report(ResolverError {
            span: Span::default(),
            kind: ResolverErrorKind::NotFound(name.path),
        });
    }

    pub fn add_value(&mut self, span: Span, path: Symbol, item: Item<Value>) {
        let namespace = self.namespaces.get_mut(self.path).unwrap();
        let old = namespace.values.insert(self.current(), item);
        if old.is_some() {
            self.report_redeclareted(span, path);
        }
    }

    pub fn add_type(&mut self, span: Span, path: Symbol, item: Item<TypeValue>) {
        let namespace = self.namespaces.get_mut(self.path).unwrap();
        let old = namespace.types.insert(self.current(), item);
        if old.is_some() {
            self.report_redeclareted(span, path);
        }
    }

    pub fn add_module(&mut self, span: Span, path: Symbol, item: Item<Symbol>) {
        let namespace = self.namespaces.get_mut(self.path).unwrap();
        let old = namespace.modules.insert(self.current(), item);
        if old.is_some() {
            self.report_redeclareted(span, path);
        }
    }

    pub fn merge(&mut self, namespace: Namespace) {
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

    pub fn resolve_module(&mut self, name: paths::Path) -> Option<Symbol> {
        let resolved = self.namespaces.resolve(self.path, name);
        match resolved {
            Resolve::ModuleNotFound(_) => {
                self.report_not_found(name);
                None
            }
            Resolve::PrivateModule(name) => {
                self.report_private(name);
                None
            }
            Resolve::ModuleFound(name) => Some(name),
        }
    }

    fn report_private(&mut self, name: Symbol) {
        self.report(ResolverError {
            span: Span::default(),
            kind: ResolverErrorKind::PrivateDefinition,
        })
    }

    fn qualify(&self, name: Symbol) -> Qualified {
        self.path.qualify(name).into()
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
        ctx.add_type(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: TypeValue::Effect(ctx.qualify(self.name.symbol())),
                parent: Some(ctx.path.symbol()),
            },
        );

        ctx.add_module(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: ctx.path.with(self.name.symbol()).symbol(),
                parent: Some(ctx.path.symbol()),
            },
        );

        ctx.scope(self.name.symbol(), |ctx| {
            for field in self.fields {
                ctx.add_value(
                    field.0.name.0.value.span.clone(),
                    field.0.name.symbol(),
                    Item {
                        visibility: field.0.visibility.clone().into(),
                        span: field.0.name.0.value.span.clone(),
                        item: Value::Effect(ctx.qualify(field.0.name.symbol())),
                        parent: Some(ctx.path.symbol()),
                    },
                );
            }
        });
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
                    item: Value::Constructor(ctx.qualify(constructor.name.symbol())),
                    parent: None,
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
                    item: Value::Field(ctx.qualify(field.name.symbol())),
                    parent: None,
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
        let name = self.name.symbol();

        ctx.add_type(
            self.name.0.value.span.clone(),
            name,
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: match self.def {
                    Some((_, TypeDef::Sum(_))) => TypeValue::Enum(ctx.qualify(name)),
                    Some((_, TypeDef::Record(_))) => TypeValue::Record(ctx.qualify(name)),
                    Some((_, TypeDef::Synonym(_))) => todo!("Synonym types are not implemented"),
                    _ => TypeValue::Abstract(ctx.qualify(name)),
                },
                parent: Some(ctx.path.symbol()),
            },
        );

        ctx.add_module(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: ctx.path.with(self.name.symbol()).symbol(),
                parent: Some(name),
            },
        );

        ctx.scope(self.name.symbol(), |ctx| {
            if let Some(some) = &self.def {
                some.1.declare(ctx);
            }
        })
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
                item: Value::Function(ctx.qualify(self.name.symbol())),
                parent: Some(ctx.path.symbol()),
            },
        );
    }
}

impl Declare for ModuleDecl {
    fn declare(&self, ctx: &mut Context) {
        ctx.add_module(
            self.name.0.value.span.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                span: self.name.0.value.span.clone(),
                item: ctx.path.symbol(),
                parent: Some(ctx.path.symbol()),
            },
        );

        ctx.scope(self.name.symbol(), |ctx| {
            if let Some(module) = &self.part {
                for top_level in &module.top_levels {
                    top_level.declare(ctx);
                }
            }
        });
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

        let Some(sub_tree) = ctx.namespaces.tree.find(ctx.path.slice()) else {
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
                    parent: None,
                },
            )
        } else {
            let namespace = ctx.namespaces.find(sub_tree.id).cloned().unwrap();
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
        ctx.scope(self.name.symbol(), |ctx| {
            if let Some(module) = &self.part {
                for top_level in module.modules() {
                    top_level.resolve_imports(ctx);
                }

                for top_level in module.uses() {
                    top_level.resolve_imports(ctx);
                }
            }
        });
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
