//! This module defines the [Check] trait that is responsible for checking an expression against
//! a type and returning an elaborated expression.

use crate::{context::Env, types::Type};

pub trait Check {
    type Out;

    fn infer(&self, typ: Type, env: Env) -> Self::Out;
}
