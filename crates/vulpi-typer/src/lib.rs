//! This is the type checker for the Vulpi language. It is responsible for checking the types of
//! the abstract syntax tree and producing a better output with semantic information that is the
//! elaborated tree. The type checker of Vulpi is based on the bidirectional type checking with
//! higher rank polymorphism and higher kinded types.

pub mod ambient;
pub mod effects;
pub mod env;
pub mod error;
pub mod kind;
pub mod module;
pub mod types;
