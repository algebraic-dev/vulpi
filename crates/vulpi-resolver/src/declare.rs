//! This is the first phase of the resolution process. It takes a syntax tree and then checks for
//! each of the modules inside of it creating an ID for each of them.

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_syntax::concrete::top_level::Visibility;
use vulpi_syntax::concrete::tree::*;

use crate::error::ResolverError;
use crate::{namespace, Context};

use crate::{
    namespace::{Item, Namespace, TypeValue, Value},
    paths,
};

use vulpi_syntax::r#abstract::Qualified;

impl<'a> Context<'a> {
    pub fn derive<T>(&mut self, name: Symbol, function: impl FnOnce(&mut Context) -> T) -> T {
        self.path.push(name);
        self.namespaces.add(self.path.clone());
        let res = function(self);
        self.path.pop();
        res
    }

    pub fn add_value(&mut self, span: Span, path: Symbol, item: Item<Value>) {
        let namespace = self.namespaces.get_mut(self.path.clone()).unwrap();
        let old = namespace.values.insert(path.clone(), item);
        if old.is_some() {
            self.report_redeclareted(span, path);
        }
    }

    pub fn add_type(&mut self, span: Span, path: Symbol, item: Item<TypeValue>) {
        let namespace = self.namespaces.get_mut(self.path.clone()).unwrap();
        let old = namespace.types.insert(path.clone(), item);
        if old.is_some() {
            self.report_redeclareted(span, path);
        }
    }

    pub fn add_module(&mut self, span: Span, path: Symbol, item: Item<Symbol>) {
        let namespace = self.namespaces.get_mut(self.path.clone()).unwrap();
        let old = namespace.modules.insert(path.clone(), item);
        if old.is_some() {
            self.report_redeclareted(span, path);
        }
    }

    pub fn register_namespace(&mut self, span: Span, path: paths::Path, namespace: Namespace) {
        let namespace = self.namespaces.add_with(path.clone(), namespace);
        if namespace.is_some() {
            self.report_redeclareted(span, path.symbol());
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

    pub fn qualify(&self, name: Symbol) -> Qualified {
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
        let name = self.name.symbol();

        ctx.add_type(
            self.name.0.value.span.clone(),
            name,
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

        ctx.derive(self.name.symbol(), |ctx| {
            for field in &self.fields {
                ctx.add_value(
                    field.name.0.value.span.clone(),
                    field.name.symbol(),
                    Item {
                        visibility: field.visibility.clone().into(),
                        span: field.name.0.value.span.clone(),
                        item: Value::Effect(ctx.qualify(field.name.symbol())),
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
            name.clone(),
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
                parent: Some(ctx.path.symbol()),
            },
        );

        ctx.derive(self.name.symbol(), |ctx| {
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
                item: ctx.path.with(self.name.symbol()).symbol(),
                parent: None,
            },
        );

        ctx.derive(self.name.symbol(), |ctx| {
            if let Some(module) = &self.part {
                for top_level in &module.top_levels {
                    top_level.declare(ctx);
                }
            } else {
                let namespaces = ctx.io.read_module(ctx.path.clone());

                match namespaces {
                    Ok(res) => res.declare(ctx),
                    Err(err) => {
                        ctx.report(ResolverError {
                            span: self.name.0.value.span.clone(),
                            kind: err,
                        });
                    }
                }
            }
        });
    }
}

impl Declare for ExternalDecl {
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
            TopLevel::External(external) => external.declare(ctx),
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
        let path = paths::Path {
            path: self
                .path
                .segments
                .iter()
                .map(|x| x.0.symbol())
                .chain(Some(self.path.last.symbol()))
                .collect(),
        };

        let Some(name) = ctx.resolve_module(self.path.span.clone(), path.clone(), path.symbol()) else {
            return;
        };

        if let Some(alias) = &self.alias {
            ctx.add_value(
                alias.alias.0.value.span.clone(),
                alias.alias.symbol(),
                Item {
                    visibility: namespace::Visibility::Public,
                    span: alias.alias.0.value.span.clone(),
                    item: Value::Module(name),
                    parent: None,
                },
            )
        } else {
            let namespace = ctx.namespaces.find(name).cloned().unwrap();
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
        ctx.derive(self.name.symbol(), |ctx| {
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
