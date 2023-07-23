//! This module defines the [Check] trait that is responsible for checking an expression against
//! a type and returning an elaborated expression.

pub mod expr;

use vulpi_location::Spanned;

use crate::{context::Env, types::Type};

impl<T: Check> Check for Spanned<T> {
    type Out = T::Out;

    fn check(&self, typ: crate::types::Type, env: crate::context::Env) -> Self::Out {
        env.set_location(self.range.clone());
        self.data.check(typ, env)
    }
}

pub trait Check {
    type Out;

    fn check(&self, typ: Type, env: Env) -> Self::Out;
}
