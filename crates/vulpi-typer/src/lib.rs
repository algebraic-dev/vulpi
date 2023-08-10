//! This is the entrypoint for the `vulpi-typer` crate. It is responsible for type checking a
//! higher rank, higher kinded, algebraic type system. It is also responsible for type inference
//! and type checking of the ambient effects system.

pub mod context;
pub mod errors;
pub mod module;
pub mod r#type;
