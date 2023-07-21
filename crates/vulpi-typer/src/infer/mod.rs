//! This module defines the [Infer] trait that is responsible for inferring the type of an
//! expression and return an elaborated expression.

use vulpi_location::{Location, Spanned};

use crate::{context::Env, types::Type};

pub mod expr;
pub mod typ;

impl<T: Infer> Infer for Spanned<T> {
    type Out = T::Out;

    fn infer(&self, env: Env) -> (Self::Out, Type) {
        env.set_location(Location {
            range: self.range.clone(),
            file: env.file,
        });
        self.data.infer(env)
    }
}

/// This trait is responsible for inferring the type of an expression and return an elaborated
/// expression.
pub trait Infer {
    type Out;

    fn infer(&self, env: Env) -> (Self::Out, Type);
}
