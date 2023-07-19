//! The unifier: this is the part of the type checker that is responsible for making two types equal
//! to each other. It is also responsible for detecting cycles in the type graph using the [occur]
//! function that checks if a type occurs in another type and other things.

use crate::{
    context::Env,
    types::{Hole, HoleInner, Mono, Type},
};

/// Unify two types together (make them equal).
pub fn unify(env: Env, left: Type, right: Type) {
    match (&*left, &*right) {
        (Mono::Variable(x), Mono::Variable(y)) if x == y => (),
        (Mono::Generalized(x), Mono::Generalized(y)) if x == y => todo!(),

        (Mono::Hole(l), Mono::Hole(r)) if l == r => (),

        (Mono::Hole(hole), _) => unify_hole(env, hole.clone(), right.clone(), false),
        (_, Mono::Hole(hole)) => unify_hole(env, hole.clone(), left.clone(), true),

        (Mono::Function(l, r), Mono::Function(l1, r1)) => {
            unify(env.clone(), l.clone(), l1.clone());
            unify(env, r.clone(), r1.clone());
        }

        _ => {}
    }
}

pub fn unify_hole(env: Env, hole: Hole, val: Type, flip: bool) {
    match hole.get() {
        HoleInner::Unbound(_, _hole_level) => {
            if occur(hole.clone(), val.clone()) {
                panic!("occur checking")
            } else {
                *hole.get_mut() = HoleInner::Link(val);
            }
        }
        HoleInner::Link(value) if flip => unify(env, val, value),
        HoleInner::Link(value) => unify(env, value, val),
    }
}

// Checks if a type occurs in another type. This is used to detect cycles in the type.
pub fn occur(hole: Hole, typ: Type) -> bool {
    match &*typ {
        Mono::Variable(_) => false,
        Mono::Generalized(_) => false,
        Mono::Error => false,
        Mono::Hole(hole_inner) => *hole_inner == hole,
        Mono::Function(l, r) => occur(hole.clone(), l.clone()) || occur(hole, r.clone()),
    }
}
