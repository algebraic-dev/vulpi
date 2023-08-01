//! This is a apply trait that is used to apply the context to the expression. It express the
//! idea of a type being applied against an expression

use crate::types::Type;

pub mod pat;

pub trait Apply {
    type Return;
    type Context<'a>;

    fn apply(&self, ty: Type, context: Self::Context<'_>) -> Self::Return;
}
