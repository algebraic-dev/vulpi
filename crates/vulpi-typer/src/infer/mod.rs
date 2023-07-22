//! This module defines the [Infer] trait that is responsible for inferring the type of an
//! expression and return an elaborated expression.

use vulpi_location::Spanned;

use crate::context::Env;

pub mod expr;
pub mod pat;
pub mod stmt;
pub mod typ;

impl<T: Infer> Infer for Spanned<T> {
    type Out = T::Out;

    fn infer(&self, env: Env) -> Self::Out {
        env.set_location(self.range.clone());
        self.data.infer(env)
    }
}

impl<T: Infer> Infer for Vec<T> {
    type Out = Vec<T::Out>;

    fn infer(&self, env: Env) -> Self::Out {
        self.iter().map(|x| x.infer(env.clone())).collect()
    }
}

impl<T: Infer> Infer for Option<T> {
    type Out = Option<T::Out>;

    fn infer(&self, env: Env) -> Self::Out {
        self.as_ref().map(|x| x.infer(env.clone()))
    }
}

/// This trait is responsible for inferring the type of an expression and return an elaborated
/// expression.
pub trait Infer {
    type Out;

    fn infer(&self, env: Env) -> Self::Out;
}
