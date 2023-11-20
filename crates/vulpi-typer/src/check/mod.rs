//! This module defines the [Check] trait that is used to check the types of the expressions and patterns
//! in the program.

use crate::{Type, Virtual};

pub mod expr;
pub mod pat;

pub trait Check {
    type Return;
    type Context<'a>;

    fn check(&self, typ: Type<Virtual>, context: Self::Context<'_>) -> Self::Return;
}
