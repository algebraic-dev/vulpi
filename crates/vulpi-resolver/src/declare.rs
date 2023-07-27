//! This is the first phase of the resolution process. It takes a syntax tree and then checks for
//! each of the modules inside of it creating an ID for each of them. After that it returns a
//! [Solver] that is an abstraction for a next phase of the resolution process, the import
//! resolution phase that will solve all the imports and then return another [Solver] that is the
//! last one.

use std::{cell::RefCell, rc::Rc};

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_report::{Diagnostic, Report};
use vulpi_syntax::concrete::tree::*;

use crate::{
    error::{ResolverError, ResolverErrorKind},
    module_tree::ModuleTree,
    namespace::{self, Item, ModuleId, Qualified, Value},
};

/// A shared counter for the module IDs.
type Counter = Rc<RefCell<usize>>;

pub struct Context<'a> {
    pub reporter: Report,
    pub module_tree: &'a mut ModuleTree,
    pub counter: Counter,
}

impl<'a> Context<'a> {
    pub fn new(reporter: Report, module_tree: &'a mut ModuleTree) -> Self {
        Self {
            reporter,
            module_tree,
            counter: Rc::new(RefCell::new(1)),
        }
    }

    fn new_with_counter(
        reporter: Report,
        module_tree: &'a mut ModuleTree,
        counter: Counter,
    ) -> Self {
        Self {
            reporter,
            module_tree,
            counter,
        }
    }

    fn report(&mut self, error: crate::error::ResolverError) {
        self.reporter.report(Diagnostic::new(error));
    }

    fn derive<'c>(&'c mut self, name: &[Symbol]) -> Context<'c> {
        let mut counter = self.counter.borrow_mut();
        let id = *counter;
        *counter += 1;
        let id = ModuleId(id);

        let tree = self.module_tree.add(name, id).unwrap();

        Context::new_with_counter(self.reporter.clone(), tree, self.counter.clone())
    }

    pub fn qualified_name(&self, name: Symbol) -> namespace::Qualified {
        namespace::Qualified {
            path: self.module_tree.id,
            name,
        }
    }

    pub fn add_value(&mut self, span: Span, name: Symbol, value: Item<Value>) {
        let old = self
            .module_tree
            .namespace
            .values
            .insert(name.clone(), value);

        if old.is_some() {
            self.report(ResolverError {
                span,
                kind: ResolverErrorKind::Redeclarated(name),
            })
        }
    }

    pub fn add_type(&mut self, span: Span, name: Symbol, value: Item<Qualified>) {
        let old = self.module_tree.namespace.types.insert(name.clone(), value);

        if old.is_some() {
            self.report(ResolverError {
                span,
                kind: ResolverErrorKind::Redeclarated(name),
            })
        }
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
            self.name.0.value.range.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                item: ctx.qualified_name(self.name.symbol()),
            },
        );

        let ctx = &mut ctx.derive(&[self.name.symbol()]);

        for field in &self.fields {
            ctx.add_value(
                field.0.name.0.value.range.clone(),
                field.0.name.symbol(),
                Item {
                    visibility: field.0.visibility.clone().into(),
                    item: Value::Effect(ctx.qualified_name(field.0.name.symbol())),
                },
            );
        }
    }
}

impl Declare for ModuleDecl {
    fn declare(&self, ctx: &mut Context) {
        let ctx = &mut ctx.derive(&[self.name.symbol()]);

        if let Some(module) = &self.part {
            for top_level in &module.top_levels {
                top_level.0.declare(ctx);
            }
        }
    }
}

impl Declare for SumDecl {
    fn declare(&self, ctx: &mut Context) {
        for constructor in self.constructors.iter() {
            ctx.add_value(
                constructor.name.0.value.range.clone(),
                constructor.name.symbol(),
                Item {
                    visibility: namespace::Visibility::Public,
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
                field.name.0.value.range.clone(),
                field.name.symbol(),
                Item {
                    visibility: namespace::Visibility::Public,
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
        ctx.add_type(
            self.name.0.value.range.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
                item: ctx.qualified_name(self.name.symbol()),
            },
        );

        let ctx = &mut ctx.derive(&[self.name.symbol()]);

        if let Some(some) = &self.def {
            some.1.declare(ctx);
        }
    }
}

impl Declare for LetDecl {
    fn declare(&self, ctx: &mut Context) {
        ctx.add_value(
            self.name.0.value.range.clone(),
            self.name.symbol(),
            Item {
                visibility: self.visibility.clone().into(),
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
