//! Type definitions for the type checker.

use crate::{
    env::Env,
    error::{TypeError, TypeErrorKind},
    kind::{Kind, KindType},
};
use std::{cell::RefCell, fmt::Display, fmt::Formatter, rc::Rc};
use vulpi_intern::Symbol;
use vulpi_syntax::{r#abstract::Qualified, r#abstract::TypeForall};

#[derive(Clone)]
pub struct Type(Rc<TypeKind>);

impl Type {
    pub fn new(ty: TypeKind) -> Self {
        Type(Rc::new(ty))
    }
}

pub enum TypeKind {
    /// The type.
    Variable(Qualified),

    /// A bounded type variable.
    Bound(usize),

    /// Rigid
    Named(Symbol),

    /// The type of a function.
    Arrow(Type, Type),

    /// The type of a type application.
    Fun(Type, Type),

    /// The type of a type abstraction.
    Forall(Symbol, Kind, Type),

    /// A empty or filled hole.
    Hole(Hole),

    /// The type of a type variable.
    Error,
}

pub enum HoleInner {
    Filled(Type),
    Empty(usize),
}

/// A hole in the type checker. This is used to represent a type that is not yet known.4
#[derive(Clone)]
pub struct Hole(pub Rc<RefCell<HoleInner>>);

impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Hole {}

impl Hole {
    pub fn new(level: usize) -> Self {
        Hole(Rc::new(RefCell::new(HoleInner::Empty(level))))
    }

    pub fn fill(&self, ty: Type) {
        *self.0.borrow_mut() = HoleInner::Filled(ty);
    }

    pub fn is_filled(&self) -> bool {
        matches!(*self.0.borrow(), HoleInner::Filled(_))
    }

    pub fn deref(&self) -> Type {
        match *self.0.borrow() {
            HoleInner::Filled(ref ty) => ty.clone(),
            HoleInner::Empty(_) => Type(Rc::new(TypeKind::Error)),
        }
    }
}

impl Type {
    pub fn print(&self, env: Env, fmt: &mut Formatter) -> std::fmt::Result {
        match *self.0 {
            TypeKind::Variable(ref name) => write!(fmt, "{}", name.name.get()),
            TypeKind::Bound(lvl) => write!(fmt, "{}", env.names[env.level - lvl - 1].get()),
            TypeKind::Named(ref name) => write!(fmt, "{}", name.get()),
            TypeKind::Arrow(ref ty1, ref ty2) => {
                write!(fmt, "(")?;
                ty1.print(env.clone(), fmt)?;
                write!(fmt, " -> ")?;
                ty2.print(env, fmt)?;
                write!(fmt, ")")
            }
            TypeKind::Fun(ref ty1, ref ty2) => {
                write!(fmt, "(")?;
                ty1.print(env.clone(), fmt)?;
                write!(fmt, " ")?;
                ty2.print(env, fmt)?;
                write!(fmt, ")")
            }
            TypeKind::Forall(ref name, ref kind, ref ty) => {
                write!(fmt, "(forall ({} : ", name.get())?;
                kind.print(fmt)?;
                write!(fmt, ") ")?;
                ty.print(env, fmt)?;
                write!(fmt, ")")
            }
            TypeKind::Hole(ref hole) => {
                if hole.is_filled() {
                    hole.deref().print(env, fmt)
                } else {
                    write!(fmt, "?")
                }
            }
            TypeKind::Error => write!(fmt, "<ERROR>"),
        }
    }

    pub fn substitute(&self, name: Symbol, to: Type) -> Type {
        match self.0.as_ref() {
            TypeKind::Named(ref name2) if name == *name2 => to,
            TypeKind::Arrow(ref from, ref to) => Type::new(TypeKind::Arrow(
                from.substitute(name.clone(), to.clone()),
                to.substitute(name, to.clone()),
            )),
            TypeKind::Fun(ref from, ref to) => Type::new(TypeKind::Fun(
                from.substitute(name.clone(), to.clone()),
                to.substitute(name, to.clone()),
            )),
            TypeKind::Forall(ref name2, ref kind, ref ty) if name != *name2 => Type::new(
                TypeKind::Forall(name2.clone(), kind.clone(), ty.substitute(name, to)),
            ),
            _ => self.clone(),
        }
    }

    pub fn instantiate(&self, env: Env, name: Symbol) -> Type {
        let hole = env.new_hole();
        self.substitute(name, hole)
    }

    pub fn generalize(&self, env: &mut Env) -> Type {
        fn generalize_over(this: Type, new_vars: &mut Vec<Symbol>, env: &mut Env) {
            match this.0.as_ref() {
                TypeKind::Arrow(ref from, ref to) => {
                    generalize_over(from.clone(), new_vars, env);
                    generalize_over(to.clone(), new_vars, env);
                }
                TypeKind::Fun(ref from, ref to) => {
                    generalize_over(from.clone(), new_vars, env);
                    generalize_over(to.clone(), new_vars, env);
                }
                TypeKind::Forall(_, _, ref ty) => {
                    generalize_over(ty.clone(), new_vars, env);
                }
                TypeKind::Hole(hole) => {
                    if hole.is_filled() {
                        generalize_over(hole.deref(), new_vars, env);
                    } else {
                        let name = env.new_name();

                        *hole.0.borrow_mut() =
                            HoleInner::Filled(Type::new(TypeKind::Named(name.clone())));

                        new_vars.push(name);
                    }
                }
                _ => (),
            }
        }

        let mut new_vars = Vec::new();
        generalize_over(self.clone(), &mut new_vars, env);

        new_vars.into_iter().fold(self.clone(), |ty, name| {
            Type::new(TypeKind::Forall(name, Kind::star(), ty))
        })
    }

    fn occurs(self, env: Env, hole: Hole, scope: usize) -> bool {
        match self.0.as_ref() {
            TypeKind::Bound(l) => {
                if *l >= scope {
                    env.report(TypeErrorKind::EscapingScope);
                    false
                } else {
                    true
                }
            }
            TypeKind::Arrow(l, r) => {
                l.clone().occurs(env.clone(), hole.clone(), scope)
                    && r.clone().occurs(env, hole, scope)
            }
            TypeKind::Fun(f, a) => {
                f.clone().occurs(env.clone(), hole.clone(), scope)
                    && a.clone().occurs(env, hole, scope)
            }
            TypeKind::Forall(_, _, a) => a.clone().occurs(env, hole, scope),
            TypeKind::Hole(hole1) => {
                if hole1.is_filled() {
                    hole1.deref().occurs(env, hole, scope);
                    true
                } else if Rc::ptr_eq(&hole.0, &hole1.0) {
                    env.report(TypeErrorKind::InfiniteType);
                    false
                } else {
                    let mut hole1 = hole1.0.borrow_mut();
                    match *hole1 {
                        HoleInner::Empty(l) => {
                            if l > scope {
                                *hole1 = HoleInner::Empty(scope);
                            }
                            true
                        }
                        HoleInner::Filled(ref mut typ1) => {
                            typ1.clone().occurs(env, hole, scope);
                            true
                        }
                    }
                }
            }
            _ => true,
        }
    }

    fn unify_hole(env: Env, hole: Hole, ty: Type) {
        if hole.is_filled() {
            Type::unify(env, hole.deref(), ty);
        } else if ty.clone().occurs(env.clone(), hole.clone(), env.level) {
            hole.fill(ty);
        }
    }

    pub fn unify(mut env: Env, left: Type, right: Type) {
        match (left.0.as_ref(), right.0.as_ref()) {
            (TypeKind::Hole(x), _) if x.is_filled() => Type::unify(env, x.deref(), right),
            (_, TypeKind::Hole(x)) if x.is_filled() => Type::unify(env, left, x.deref()),

            (TypeKind::Hole(x), _) => Type::unify_hole(env, x.clone(), right),

            (TypeKind::Bound(x), TypeKind::Bound(y)) if x == y => (),
            (TypeKind::Variable(ref name1), TypeKind::Variable(ref name2)) if name1 == name2 => (),
            (TypeKind::Named(ref name1), TypeKind::Named(ref name2)) if name1 == name2 => (),
            (TypeKind::Fun(a, b), TypeKind::Fun(c, d)) => {
                Type::unify(env.clone(), a.clone(), c.clone());
                Type::unify(env, b.clone(), d.clone());
            }
            (TypeKind::Forall(na, _, ta), TypeKind::Forall(nb, _, tb)) => {
                let name = env.new_name();
                env.names.push_back(name);
                env.level += 1;

                let ty = Type::new(TypeKind::Bound(env.level));

                Type::unify(
                    env.clone(),
                    ta.substitute(na.clone(), ty.clone()),
                    tb.substitute(nb.clone(), ty),
                );
            }
            (TypeKind::Error, _) | (_, TypeKind::Error) => (),
            _ => env.report(TypeErrorKind::TypeMismatch(env.clone(), left, right)),
        }
    }

    pub fn show(&self, env: Env) -> ShowPair {
        ShowPair(env, self.clone())
    }
}

pub struct ShowPair(Env, Type);

impl Display for ShowPair {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        self.1.print(self.0.clone(), fmt)
    }
}
