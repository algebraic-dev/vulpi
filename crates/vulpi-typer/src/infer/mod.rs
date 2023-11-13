//! The inference trait. It correponds to the `Γ |- e ⇒ A -| ∆` on the paper. It is responsible for
//! inferring the type of an expression given a context.

pub mod expr;
pub mod kind;
pub mod literal;
pub mod pat;
pub mod r#type;

/// The inference trait. It descovers the type of an expression based on the context.
pub trait Infer {
    type Return;
    type Context<'a>;

    fn infer(&self, context: Self::Context<'_>) -> Self::Return;
}

impl<T: Infer> Infer for Option<T> {
    type Return = Option<T::Return>;
    type Context<'a> = T::Context<'a>;

    fn infer(&self, context: Self::Context<'_>) -> Self::Return {
        self.as_ref().map(|x| x.infer(context))
    }
}

impl<T: Infer> Infer for Box<T> {
    type Return = Box<T::Return>;
    type Context<'a> = T::Context<'a>;
    fn infer(&self, context: Self::Context<'_>) -> Self::Return {
        Box::new(self.as_ref().infer(context))
    }
}
