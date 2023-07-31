//! The inference trait. It correponds to the `Γ |- e ⇒ A -| ∆` on the paper. It is responsible for
//! inferring the type of an expression given a context.

pub mod kind;
pub mod types;

/// The inference trait. It descovers the type of an expression based on the context.
pub trait Infer {
    type Return;
    type Context;

    fn infer(&self, context: &Self::Context) -> Self::Return;
}

impl<T: Infer> Infer for Option<T> {
    type Return = Option<T::Return>;
    type Context = T::Context;

    fn infer(&self, context: &Self::Context) -> Self::Return {
        self.as_ref().map(|x| x.infer(context))
    }
}

impl<T: Infer> Infer for Vec<T> {
    type Return = Vec<T::Return>;
    type Context = T::Context;

    fn infer(&self, context: &Self::Context) -> Self::Return {
        self.iter().map(|x| x.infer(context)).collect()
    }
}

impl<T: Infer> Infer for Box<T> {
    type Return = Box<T::Return>;
    type Context = T::Context;

    fn infer(&self, context: &Self::Context) -> Self::Return {
        Box::new(self.as_ref().infer(context))
    }
}
