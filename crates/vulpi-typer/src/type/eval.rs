//! Module for evaluation and quotation of real and virtual [Type]s. The main difference between
//! the two is that virtual types contain closures and can be executed while real types are just
//! types.

use super::{
    r#virtual, r#virtual::TypingEnv, r#virtual::Virtual, real::Real, Hole, HoleInner, Type,
    TypeKind,
};

/// Trait for evaluation of types.
pub trait Eval<T> {
    fn eval(&self, env: &TypingEnv) -> T;
}

impl Eval<Type<Virtual>> for Type<Real> {
    fn eval(&self, env: &TypingEnv) -> Type<Virtual> {
        match self.as_ref() {
            TypeKind::Pi(pi) => Type::new(TypeKind::Pi(r#virtual::Pi {
                ty: pi.ty.clone().eval(env),
                body: r#virtual::Closure {
                    env: env.clone(),
                    body: pi.body.clone(),
                },
            })),
            TypeKind::Forall(f) => Type::new(TypeKind::Forall(r#virtual::Forall {
                name: f.name.clone(),
                ty: f.ty.clone().eval(env),
                body: r#virtual::Closure {
                    env: env.clone(),
                    body: f.body.clone(),
                },
            })),
            TypeKind::Type => Type::new(TypeKind::Type),
            TypeKind::Hole(r) => r.eval(env),
            TypeKind::Variable(v) => Type::new(TypeKind::Variable(v.clone())),
            TypeKind::Bound(v) => Type::new(TypeKind::Bound(*v)),
            TypeKind::Tuple(v) => Type::new(TypeKind::Tuple(v.clone().eval(env))),
            TypeKind::Application(v, u) => Type::new(TypeKind::Application(
                v.clone().eval(env),
                u.clone().eval(env),
            )),
            TypeKind::Empty => Type::new(TypeKind::Empty),
            TypeKind::Extend(l, t, u) => Type::new(TypeKind::Extend(
                l.clone(),
                t.clone().eval(env),
                u.clone().eval(env),
            )),
            TypeKind::Error => Type::new(TypeKind::Error),
        }
    }
}

impl Eval<Vec<Type<Virtual>>> for Vec<Type<Real>> {
    fn eval(&self, env: &TypingEnv) -> Vec<Type<Virtual>> {
        self.iter().map(|v| v.eval(env)).collect()
    }
}

impl Eval<Type<Virtual>> for Hole<Real> {
    fn eval(&self, env: &TypingEnv) -> Type<Virtual> {
        match &*self.0.borrow() {
            HoleInner::Empty(l) => Type::new(TypeKind::Hole(Hole::empty(*l))),
            HoleInner::Row(l, r) => Type::new(TypeKind::Hole(Hole::row(*l, r.clone()))),
            HoleInner::Filled(f) => f.clone().eval(env),
        }
    }
}
