//! This is a check trait that checks if an expression matches a type.

use crate::{Type, Virtual};

pub mod expr;
pub mod pat;

pub trait Check {
    type Return;
    type Context<'a>;

    fn check(&self, ty: Type<Virtual>, context: Self::Context<'_>) -> Self::Return;
}
