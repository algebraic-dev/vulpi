//! The unifier: this is the part of the type checker that is responsible for making two types equal
//! to each other. It is also responsible for detecting cycles in the type graph using the [occur]
//! function that checks if a type occurs in another type and other things.

use crate::{
    context::Env,
    types::{Hole, HoleInner, Kind, Mono, Type},
};

pub fn unify_kinds(env: Env, left: Kind, right: Kind) {
    if !left.is_error() && !right.is_error() && left != right {
        env.report(crate::error::TypeErrorKind::MismatchKind(left, right))
    }
}

pub fn unify(env: Env, left: Type, right: Type) {
    if !unify_types(env.clone(), left.clone(), right.clone()) {
        env.report(crate::error::TypeErrorKind::Mismatch(left, right))
    }
}

/// Unify two types together (make them equal).
fn unify_types(env: Env, left: Type, right: Type) -> bool {
    match (&*left, &*right) {
        (Mono::Variable(p, x), Mono::Variable(p1, y)) if p == p1 && x == y => true,
        (Mono::Generalized(x, _), Mono::Generalized(y, _)) if x == y => true,
        (Mono::Unit, Mono::Unit) => true,

        (Mono::Bound(x), Mono::Bound(y)) if x == y => true,

        (Mono::Hole(l), Mono::Hole(r)) if l == r => true,

        (Mono::Hole(hole), _) => unify_hole(env, hole.clone(), right.clone(), false),
        (_, Mono::Hole(hole)) => unify_hole(env, hole.clone(), left.clone(), true),

        (Mono::Error, _) => true,
        (_, Mono::Error) => true,

        (Mono::Function(l, r), Mono::Function(l1, r1)) => {
            unify_types(env.clone(), l.clone(), l1.clone())
                && unify_types(env, r.clone(), r1.clone())
        }

        (Mono::Application(f, a), Mono::Application(f1, a1)) => {
            unify_types(env.clone(), f.clone(), f1.clone())
                && unify_types(env, a.clone(), a1.clone())
        }

        _ => false,
    }
}

pub fn unify_hole(env: Env, hole: Hole, val: Type, flip: bool) -> bool {
    match hole.get() {
        HoleInner::Unbound(_, _hole_level) => {
            if occur(hole.clone(), val.clone()) {
                panic!("occur checking")
            } else {
                *hole.get_mut() = HoleInner::Link(val);
                true
            }
        }
        HoleInner::Link(value) if flip => unify_types(env, val, value),
        HoleInner::Link(value) => unify_types(env, value, val),
    }
}

// Checks if a type occurs in another type. This is used to detect cycles in the type.
pub fn occur(hole: Hole, typ: Type) -> bool {
    match &*typ {
        Mono::Unit => false,
        Mono::Variable(_, _) => false,
        Mono::Generalized(_, _) => false,
        Mono::Bound(_) => false,
        Mono::Error => false,
        Mono::Hole(hole_inner) => *hole_inner == hole,
        Mono::Function(l, r) => occur(hole.clone(), l.clone()) || occur(hole, r.clone()),
        Mono::Application(f, a) => occur(hole.clone(), f.clone()) || occur(hole, a.clone()),
    }
}
