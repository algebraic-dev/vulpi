//! This is the entrypoint for the `vulpi-typer` crate. It is responsible for type checking a
//! higher rank, higher kinded, algebraic type system. It is also responsible for type inference
//! and type checking of the ambient effects system.

use context::Context;
use r#type::r#virtual::Env;

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
