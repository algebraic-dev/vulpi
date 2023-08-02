//! This is a check trait that checks if an expression matches a type.

pub mod expr;

use crate::types::Type;

pub trait Check {
    type Return;
    type Context<'a>;

    fn check(&self, ty: Type, context: Self::Context<'_>) -> Self::Return;
}
