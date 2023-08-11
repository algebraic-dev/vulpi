//! This is the entrypoint for the `vulpi-typer` crate. It is responsible for type checking a
//! higher rank, higher kinded, algebraic type system. It is also responsible for type inference
//! and type checking of the ambient effects system.

use crate::r#type::eval::Quote;
pub use context::Context;
use infer::Infer;
use module::{Def, TypeData};
pub use r#type::{r#virtual::Env, Type};
use r#type::{real::Real, Kind};
use vulpi_syntax::r#abstract::*;

use crate::r#type::eval::Eval;

pub mod context;
pub mod errors;
pub mod infer;
pub mod module;
pub mod r#type;

/// Trait for declaration of top level items inside the type checker.
pub trait Declare {
    fn declare(&self, context: (&mut Context, Env));
    fn define(&self, _context: (&mut Context, Env)) {}
}

impl Declare for TypeDecl {
    fn declare(&self, (context, env): (&mut Context, Env)) {
        let mut binders = Vec::new();

        let vec = &self.binders;
        for binder in vec {
            match binder {
                TypeBinder::Implicit(_) => {
                    binders.push(context.hole(&env, Type::typ()).quote(env.level))
                }
                TypeBinder::Explicit(_, m) => binders.push(m.infer(env.clone())),
            }
        }

        let size = binders.len();
        let kind = Type::<Real>::function(binders, Type::typ());

        let type_def = &self.def;
        let def = get_definition_of_type(type_def);

        context.modules.get(&self.name.path).types.insert(
            self.name.name.clone(),
            TypeData {
                kind,
                binders: size,
                module: self.namespace.clone(),
                def,
            },
        );
    }
}

fn get_definition_of_type(type_def: &TypeDef) -> Def {
    match type_def {
        TypeDef::Sum(cons) => Def::Enum(cons.constructors.iter().map(|x| x.name.clone()).collect()),
        TypeDef::Record(rec) => Def::Record(rec.fields.iter().map(|x| x.0.clone()).collect()),
        TypeDef::Synonym(_) => Def::Type,
        TypeDef::Abstract => Def::Type,
    }
}

impl Declare for ExternalDecl {
    fn declare(&self, (ctx, mut env): (&mut Context, Env)) {
        let fvs = self.ty.data.free_variables();

        for fv in fvs {
            env = env.add(Some(fv), ctx.hole(&env, Type::typ()));
        }

        let (typ, k) = self.ty.infer((ctx, env.clone()));
        ctx.subsumes(env.clone(), k, Kind::typ());

        println!("{}", typ.show(&env));
        println!("{}", typ.eval(&env).quote(env.level).show(&env));

        ctx.modules
            .get(&self.namespace)
            .variables
            .insert(self.name.clone(), typ);
    }
}

impl Declare for ModuleDecl {
    fn declare(&self, (ctx, env): (&mut Context, Env)) {
        if let Some(types) = self.types() {
            for decl in types {
                decl.declare((ctx, env.clone()));
            }
        }

        if let Some(externals) = self.externals() {
            for decl in externals {
                decl.declare((ctx, env.clone()));
            }
        }
    }
}

impl Declare for Module {
    fn declare(&self, (ctx, env): (&mut Context, Env)) {
        for module in self.modules() {
            module.declare((ctx, env.clone()));
        }

        for decl in self.types() {
            decl.declare((ctx, env.clone()));
        }

        for externals in self.externals() {
            externals.declare((ctx, env.clone()));
        }
    }
}
