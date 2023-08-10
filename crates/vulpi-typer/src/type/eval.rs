//! Module for evaluation and quotation of real and virtual [Type]s. The main difference between
//! the two is that virtual types contain closures and can be executed while real types are just
//! types.

use super::{
    r#virtual,
    r#virtual::Env,
    r#virtual::Virtual,
    real::{self, Real},
    Hole, HoleInner, Level, Type, TypeKind,
};

/// Trait for evaluation of types.
pub trait Eval<T> {
    fn eval(&self, env: &Env) -> T;
}

impl Eval<Type<Virtual>> for Type<Real> {
    fn eval(&self, env: &Env) -> Type<Virtual> {
        match self.as_ref() {
            TypeKind::Pi(pi) => Type::new(TypeKind::Pi(r#virtual::Pi {
                ty: pi.ty.clone().eval(env),
                effs: pi.effs.clone().eval(env),
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
            TypeKind::Effect => Type::new(TypeKind::Effect),
            TypeKind::Hole(r) => r.eval(env),
            TypeKind::Variable(v) => Type::new(TypeKind::Variable(v.clone())),
            TypeKind::Bound(v) => env.types[v.0].clone(),
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
    fn eval(&self, env: &Env) -> Vec<Type<Virtual>> {
        self.iter().map(|v| v.eval(env)).collect()
    }
}

impl Eval<Type<Virtual>> for Hole<Real> {
    fn eval(&self, env: &Env) -> Type<Virtual> {
        match &*self.0.borrow() {
            HoleInner::Empty(l) => Type::new(TypeKind::Hole(Hole::empty(*l))),
            HoleInner::Row(l, r) => Type::new(TypeKind::Hole(Hole::row(*l, r.clone()))),
            HoleInner::Filled(f) => f.clone().eval(env),
        }
    }
}

/// Quotation of types.
pub trait Quote<T> {
    fn quote(&self, lvl: Level) -> T;
}

impl Quote<Type<Real>> for Hole<Virtual> {
    fn quote(&self, lvl: Level) -> Type<Real> {
        match &*self.0.borrow() {
            HoleInner::Empty(l) => Type::new(TypeKind::Hole(Hole::empty(*l))),
            HoleInner::Row(l, r) => Type::new(TypeKind::Hole(Hole::row(*l, r.clone()))),
            HoleInner::Filled(f) => f.clone().quote(lvl),
        }
    }
}

impl Quote<Vec<Type<Real>>> for Vec<Type<Virtual>> {
    fn quote(&self, lvl: Level) -> Vec<Type<Real>> {
        self.iter().map(|v| v.quote(lvl)).collect()
    }
}

impl Quote<Type<Real>> for Type<Virtual> {
    fn quote(&self, depth: Level) -> Type<Real> {
        match self.as_ref() {
            TypeKind::Type => Type::new(TypeKind::Type),
            TypeKind::Effect => Type::new(TypeKind::Effect),
            TypeKind::Pi(pi) => Type::new(TypeKind::Pi(real::Pi {
                ty: pi.ty.clone().quote(depth),
                effs: pi.effs.clone().quote(depth),
                body: pi
                    .body
                    .apply(None, Type::new(TypeKind::Bound(depth)))
                    .quote(depth.inc()),
            })),
            TypeKind::Forall(f) => Type::new(TypeKind::Forall(real::Forall {
                name: f.name.clone(),
                ty: f.ty.clone().quote(depth),
                body: f
                    .body
                    .apply(Some(f.name.clone()), Type::new(TypeKind::Bound(depth)))
                    .quote(depth.inc()),
            })),
            TypeKind::Hole(h) => h.quote(depth),
            TypeKind::Variable(v) => Type::new(TypeKind::Variable(v.clone())),
            TypeKind::Bound(i) => Type::new(TypeKind::Bound(Level::to_index(depth, *i))),
            TypeKind::Tuple(p) => Type::new(TypeKind::Tuple(p.quote(depth))),
            TypeKind::Application(func, arg) => {
                let func = func.quote(depth);
                let arg = arg.quote(depth);
                Type::new(TypeKind::Application(func, arg))
            }
            TypeKind::Empty => Type::new(TypeKind::Empty),
            TypeKind::Extend(label, t, u) => Type::new(TypeKind::Extend(
                label.clone(),
                t.clone().quote(depth),
                u.clone().quote(depth),
            )),
            TypeKind::Error => Type::new(TypeKind::Error),
        }
    }
}
