//! This module defines the [Infer] trait that is responsible for inferring the type of an
//! expression and return an elaborated expression.

use crate::types::Type;

pub mod expr;

/// This trait is responsible for inferring the type of an expression and return an elaborated
/// expression.
pub trait Infer {
    fn infer(&self) -> Type;
}
