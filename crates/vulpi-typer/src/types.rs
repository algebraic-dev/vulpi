//! Type definitions for the type checker. This module includes a lot of operations that are
//! required for higher rank polymorphism and higher kinded types like unification, instantiation,
//! generalization, subsumption, etc.

use crate::{env::Env, error::TypeErrorKind, kind::Kind};
use std::{cell::RefCell, collections::HashSet, fmt::Display, fmt::Formatter, hash::Hash, rc::Rc};
use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract::Qualified;

/// A type is simply a wrapper around [TypeKind] that allows for sharing. This is the main type
/// for the type checker and this type allows for some interesting properties like:
///
/// - Higher rank polymorphism
/// - Higher kinded types
/// - Algebraic Effects
///
#[derive(Clone)]
pub struct Type(Rc<TypeKind>);

impl AsRef<TypeKind> for Type {
    fn as_ref(&self) -> &TypeKind {
        &self.0
    }
}

impl Type {
    pub fn new(ty: TypeKind) -> Self {
        Type(Rc::new(ty))
    }

    pub fn lacks(labels: HashSet<Qualified>) -> Self {
        Type::new(TypeKind::Hole(Hole::lacks(labels)))
    }

    pub fn hole(level: usize, kind: Kind) -> Type {
        Type::new(TypeKind::Hole(Hole::new(level, kind)))
    }

    pub fn arrow(left: Type, effs: Type, right: Type) -> Type {
        Type::new(TypeKind::Arrow(left, effs, right))
    }

    pub fn empty_row() -> Type {
        Type::new(TypeKind::RowEmpty)
    }

    pub fn extend_row(label: Qualified, ty: Type, rest: Type) -> Type {
        Type::new(TypeKind::RowExtend(label, ty, rest))
    }

    pub fn tuple(types: Vec<Type>) -> Type {
        Type::new(TypeKind::Tuple(types))
    }

    pub fn forall(name: Symbol, kind: Kind, ty: Type) -> Type {
        Type::new(TypeKind::Forall(name, kind, ty))
    }

    pub fn app(left: Type, right: Vec<Type>) -> Type {
        let mut ty = left;
        for arg in right {
            ty = Type::new(TypeKind::App(ty, arg));
        }
        ty
    }

    pub fn variable(name: Qualified) -> Type {
        Type::new(TypeKind::Variable(name))
    }

    pub fn named(name: Symbol) -> Type {
        Type::new(TypeKind::Named(name))
    }

    pub fn error() -> Type {
        Type::new(TypeKind::Error)
    }

    pub fn destruct(&self) -> Option<(Qualified, Vec<Type>)> {
        match self.deref().as_ref() {
            TypeKind::App(left, right) => {
                let mut args = vec![right.clone()];

                let mut ty = left.clone();

                while let TypeKind::App(left, right) = ty.deref().as_ref() {
                    args.push(right.clone());
                    ty = left.clone();
                }

                match ty.deref().as_ref() {
                    TypeKind::Variable(qualified) => {
                        args.reverse();
                        Some((qualified.clone(), args))
                    }
                    _ => None,
                }
            }
            TypeKind::Variable(qualified) => Some((qualified.clone(), vec![])),
            _ => None,
        }
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
    Arrow(Type, Type, Type),

    /// The type of a type application.
    App(Type, Type),

    /// The type of a type abstraction.
    Forall(Symbol, Kind, Type),

    /// The type of a tuple.
    Tuple(Vec<Type>),

    /// A empty or filled hole.
    Hole(Hole),

    /// Row Empty for algebraic effects that are row polymorphic
    RowEmpty,

    /// Row Extend for algebraic effects that are row polymorphic
    RowExtend(Qualified, Type, Type),

    /// The type of a type variable.
    Error,
}

/// Inside of a hole, it can be empty or filled wth a type. The Empty version stores the level where
/// it was created. This is used to determine if it's escaping or not.
#[derive(Clone)]
pub enum HoleInner {
    Filled(Type),
    Lacks(HashSet<Qualified>),
    Empty(usize, Kind),
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
    pub fn new(level: usize, kind: Kind) -> Self {
        Hole(Rc::new(RefCell::new(HoleInner::Empty(level, kind))))
    }

    pub fn lacks(labels: HashSet<Qualified>) -> Self {
        Hole(Rc::new(RefCell::new(HoleInner::Lacks(labels))))
    }

    pub fn fill(&self, ty: Type) {
        *self.0.borrow_mut() = HoleInner::Filled(ty);
    }

    pub fn is_filled(&self) -> bool {
        matches!(*self.0.borrow(), HoleInner::Filled(_))
    }

    pub fn is_lacks(&self) -> bool {
        matches!(*self.0.borrow(), HoleInner::Lacks(_))
    }

    pub fn deref(&self) -> Type {
        match *self.0.borrow() {
            HoleInner::Filled(ref ty) => ty.clone(),
            HoleInner::Empty(_, _) => Type(Rc::new(TypeKind::Hole(self.clone()))),
            HoleInner::Lacks(_) => Type(Rc::new(TypeKind::Hole(self.clone()))),
        }
    }
}

impl Type {
    pub fn deref(&self) -> Type {
        match *self.0 {
            TypeKind::Hole(ref hole) => hole.deref(),
            _ => self.clone(),
        }
    }
    fn print(&self, env: Env, fmt: &mut Formatter) -> std::fmt::Result {
        match *self.0 {
            TypeKind::Variable(ref name) => write!(fmt, "~{}", name.name.get()),
            TypeKind::Bound(lvl) => write!(fmt, "!{}", env.names[env.level - lvl - 1].0.get()),
            TypeKind::Named(ref name) => write!(fmt, "^{}", name.get()),
            TypeKind::Arrow(ref ty1, ref e, ref ty2) => {
                write!(fmt, "(")?;
                ty1.print(env.clone(), fmt)?;
                write!(fmt, " -> ")?;

                if !matches!(e.deref().as_ref(), TypeKind::RowEmpty) {
                    write!(fmt, "{{")?;
                    e.print(env.clone(), fmt)?;
                    write!(fmt, "}}")?;
                }

                ty2.print(env, fmt)?;
                write!(fmt, ")")
            }
            TypeKind::App(ref ty1, ref ty2) => {
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
            TypeKind::Tuple(ref tys) => {
                write!(fmt, "(")?;
                for (i, ty) in tys.iter().enumerate() {
                    ty.print(env.clone(), fmt)?;
                    if i != tys.len() - 1 {
                        write!(fmt, ", ")?;
                    }
                }
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
            TypeKind::RowEmpty => "{}".fmt(fmt),
            TypeKind::RowExtend(_, ref t, ref n) => {
                write!(fmt, "|")?;
                t.print(env.clone(), fmt)?;
                write!(fmt, ", ")?;
                n.print(env, fmt)
            }
        }
    }

    pub fn substitute(&self, name: Symbol, to: Type) -> Type {
        match self.0.as_ref() {
            TypeKind::Named(ref name2) if name == *name2 => to,
            TypeKind::Arrow(ref from, ref e, ref r) => Type::new(TypeKind::Arrow(
                from.substitute(name.clone(), to.clone()),
                e.substitute(name.clone(), to.clone()),
                r.substitute(name, to),
            )),
            TypeKind::App(ref from, ref p) => Type::new(TypeKind::App(
                from.substitute(name.clone(), to.clone()),
                p.substitute(name, to),
            )),
            TypeKind::Forall(ref name2, ref kind, ref ty) if name != *name2 => Type::new(
                TypeKind::Forall(name2.clone(), kind.clone(), ty.substitute(name, to)),
            ),
            TypeKind::Tuple(ref tys) => Type::new(TypeKind::Tuple(
                tys.iter()
                    .map(|ty| ty.substitute(name.clone(), to.clone()))
                    .collect(),
            )),
            _ => self.clone(),
        }
    }

    pub fn instantiate(&self, env: Env, name: Symbol, k: Kind) -> Type {
        let hole = env.new_hole(k);
        self.substitute(name, hole)
    }

    /// Generalizes a type by replacing all free type variables with a type variable.
    pub fn generalize(&self, env: &mut Env) -> Type {
        fn generalize_over(this: Type, new_vars: &mut Vec<Symbol>, env: &mut Env) {
            match this.0.as_ref() {
                TypeKind::Arrow(ref from, ref e, ref to) => {
                    generalize_over(from.clone(), new_vars, env);
                    generalize_over(e.clone(), new_vars, env);
                    generalize_over(to.clone(), new_vars, env);
                }
                TypeKind::App(ref from, ref to) => {
                    generalize_over(from.clone(), new_vars, env);
                    generalize_over(to.clone(), new_vars, env);
                }
                TypeKind::Forall(_, _, ref ty) => {
                    generalize_over(ty.clone(), new_vars, env);
                }
                TypeKind::Tuple(ref tys) => {
                    for ty in tys {
                        generalize_over(ty.clone(), new_vars, env);
                    }
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
            TypeKind::Arrow(l, e, r) => {
                l.clone().occurs(env.clone(), hole.clone(), scope)
                    && e.clone().occurs(env.clone(), hole.clone(), scope)
                    && r.clone().occurs(env, hole, scope)
            }
            TypeKind::App(f, a) => {
                f.clone().occurs(env.clone(), hole.clone(), scope)
                    && a.clone().occurs(env, hole, scope)
            }
            TypeKind::Tuple(tys) => tys
                .iter()
                .all(|ty| ty.clone().occurs(env.clone(), hole.clone(), scope)),
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
                    match &*hole1 {
                        HoleInner::Empty(l, k) => {
                            if *l > scope {
                                *hole1 = HoleInner::Empty(scope, k.clone());
                            }
                            true
                        }
                        HoleInner::Filled(typ1) => {
                            typ1.clone().occurs(env, hole, scope);
                            true
                        }
                        HoleInner::Lacks(_) => todo!(),
                    }
                }
            }
            _ => true,
        }
    }

    fn unify_hole(env: Env, hole: Hole, ty: Type, flipped: bool) {
        if hole.is_filled() {
            if flipped {
                Type::unify(env, ty, hole.deref());
            } else {
                Type::unify(env, hole.deref(), ty);
            }
        } else if hole.is_lacks() {
            let HoleInner::Lacks(lacks) = hole.0.borrow().clone() else { unreachable!() };

            let (list, mv) = &ty.to_list();
            let list = list.iter().map(|x| x.0.clone()).collect::<HashSet<_>>();

            let intersection = lacks.intersection(&list).collect::<Vec<_>>();

            if intersection.is_empty() && mv.is_none() {
                hole.fill(ty);
            } else if let Some(hole2) = mv {
                let HoleInner::Lacks(lacks1) = hole2.0.borrow().clone() else { unreachable!() };
                let union = lacks.union(&lacks1).cloned().collect();
                hole.fill(ty);
                hole2.fill(Type::lacks(union))
            } else {
                unreachable!()
            }
        } else {
            if let TypeKind::Hole(hole1) = ty.0.as_ref() {
                if hole == *hole1 {
                    return;
                }
            }

            if ty.clone().occurs(env.clone(), hole.clone(), env.level) {
                hole.fill(ty);
            }
        }
    }

    pub fn rewrite_row(env: Env, typ: Type, new_label: Qualified) -> (Type, Type) {
        match typ.deref().as_ref() {
            TypeKind::RowEmpty => panic!("cannot insert"),
            TypeKind::RowExtend(label, field_ty, row_tail) => {
                if *label == new_label {
                    (field_ty.clone(), row_tail.clone())
                } else if let TypeKind::Hole(hole) = row_tail.deref().as_ref() {
                    let beta = Type::lacks(vec![new_label.clone()].into_iter().collect());
                    let gamma = env.new_hole(Kind::effect());

                    Type::unify_hole(
                        env,
                        hole.clone(),
                        Type::extend_row(new_label, gamma.deref(), beta.clone()),
                        false,
                    );

                    (
                        gamma,
                        Type::extend_row(label.clone(), field_ty.clone(), beta),
                    )
                } else {
                    let (field_ty2, row_tail2) =
                        Type::rewrite_row(env, row_tail.clone(), new_label);
                    (
                        field_ty2,
                        Type::extend_row(label.clone(), field_ty.clone(), row_tail2),
                    )
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn unify(mut env: Env, left: Type, right: Type) {
        match (left.0.as_ref(), right.0.as_ref()) {
            (TypeKind::Hole(x), _) if x.is_filled() => Type::unify(env, x.deref(), right),
            (_, TypeKind::Hole(x)) if x.is_filled() => Type::unify(env, left, x.deref()),
            (TypeKind::RowEmpty, TypeKind::RowEmpty) => (),

            //
            //     α /∈ FV(A)     Γ [^α] ⊢ ^α := A ⊣ ∆
            // ------------------------------------------ :InstantiateL
            //           Γ [^α] ⊢ ^α <: A ⊣ ∆
            //
            (TypeKind::Hole(x), _) => Type::unify_hole(env, x.clone(), right, false),

            //
            //    α /∈ FV(A)    Γ [^α] ⊢ A := ^α ⊣ ∆
            // ---------------------------------------- :InstantiateR
            //          Γ [^α] ⊢ A <: ^α ⊣ ∆
            //
            (_, TypeKind::Hole(x)) => Type::unify_hole(env, x.clone(), left, true),

            (TypeKind::Bound(x), TypeKind::Bound(y)) if x == y => (),
            (TypeKind::Variable(ref name1), TypeKind::Variable(ref name2)) if name1 == name2 => (),
            (TypeKind::Named(ref name1), TypeKind::Named(ref name2)) if name1 == name2 => (),

            (TypeKind::Tuple(ref tys1), TypeKind::Tuple(ref tys2)) if tys1.len() == tys2.len() => {
                for (ty1, ty2) in tys1.iter().zip(tys2.iter()) {
                    Type::unify(env.clone(), ty1.clone(), ty2.clone());
                }
            }

            (TypeKind::App(a, b), TypeKind::App(c, d)) => {
                Type::unify(env.clone(), a.clone(), c.clone());
                Type::unify(env, b.clone(), d.clone());
            }

            (TypeKind::Arrow(a, e, b), TypeKind::Arrow(c, f, d)) => {
                Type::unify(env.clone(), a.clone(), c.clone());
                Type::unify(env.clone(), e.clone(), f.clone());
                Type::unify(env, b.clone(), d.clone());
            }

            (TypeKind::Forall(na, k, ta), TypeKind::Forall(nb, k1, tb)) => {
                let name = env.new_name();

                k.unify(&env, k1);

                env.names.push_back((name, k.clone()));
                env.level += 1;

                let ty = Type::new(TypeKind::Bound(env.level));

                Type::unify(
                    env.clone(),
                    ta.substitute(na.clone(), ty.clone()),
                    tb.substitute(nb.clone(), ty),
                );
            }

            (TypeKind::RowExtend(label1, field_ty1, row_tail1), TypeKind::RowExtend(_, _, _)) => {
                let (field_ty2, row_tail2) =
                    Type::rewrite_row(env.clone(), right.clone(), label1.clone());

                // Needs occur check of these things sob sob
                Type::unify(env.clone(), field_ty1.clone(), field_ty2);
                Type::unify(env.clone(), row_tail1.clone(), row_tail2);
            }

            (TypeKind::Error, _) | (_, TypeKind::Error) => (),
            _ => env.report(TypeErrorKind::TypeMismatch(env.clone(), left, right)),
        }
    }

    fn sub_hole_type(env: Env, hole: Hole, ty: Type) {
        match ty.0.as_ref() {
            //
            // Γ [^α], β ⊢ ^α :=< B ⊣ ∆, β, ∆'
            // ------------------------------- InstLAllR
            //    Γ [^α] ⊢ ^α :=< ∀β. B ⊣ ∆
            //
            TypeKind::Forall(_, kind, t) => {
                let env = env.add_new_ty(kind.clone());
                Type::sub_hole_type(env, hole, t.clone());
            }
            //
            // Γ [^α2, ^α1, ^α = ^α1 → ^α2] ⊢ A1 =<: ^α1 a Θ Θ ⊣ ^α2 :=< [Θ]A2 a ∆
            // ------------------------------------------------------------------- InstLArr
            //                    Γ [^α] ⊢ ^α :=< A1 → A2 ⊣ ∆
            TypeKind::Arrow(a, e, b) => {
                let HoleInner::Empty(scope, k) = hole.0.borrow().clone() else { unreachable!() };

                let hole_a = Hole::new(scope, k.clone());
                let hole_e = Hole::new(scope, Kind::effect());
                let hole_b = Hole::new(scope, k);

                hole.fill(Type::arrow(
                    Type::new(TypeKind::Hole(hole_a.clone())),
                    Type::new(TypeKind::Hole(hole_e.clone())),
                    Type::new(TypeKind::Hole(hole_b.clone())),
                ));

                Type::sub_type_hole(env.clone(), a.clone(), hole_a);
                Type::sub_hole_type(env.clone(), hole_b, b.clone());
                Type::unify_hole(env, hole_e, e.clone(), false);
            }

            _ => Type::unify_hole(env, hole, ty, false),
        }
    }

    fn sub_type_hole(env: Env, ty: Type, hole: Hole) {
        match ty.0.as_ref() {
            //
            //      Γ [^α], ▶^β, ^β ⊢ [^β/β]B =<: ^α ⊣ ∆, ▶^β, ∆′
            // ----------------------------------------------------  InstRAllL
            //              Γ [^α] ⊢ ∀β. B =<: ^α ⊣ ∆
            //
            TypeKind::Forall(n, k, t) => {
                let a_1 = t.instantiate(env.clone(), n.clone(), k.clone());
                Type::sub_type_hole(env, a_1, hole)
            }

            //
            // Γ [^α2, ^α1, ^α = ^α1 → ^α2] ⊢ ^α1 :=< A1 ⊣ Θ    Θ ⊢ [Θ]A2 =<: ^α2 ⊣ ∆
            // ----------------------------------------------------------------------- InstRArr
            //                     Γ [^α] ⊢ A1 → A2 =<: ^α ⊣ ∆
            //
            TypeKind::Arrow(a, e, b) => {
                let HoleInner::Empty(scope, k) = hole.0.borrow().clone() else { unreachable!() };

                let hole_a = Hole::new(scope, k.clone());
                let hole_e = Hole::new(scope, Kind::effect());
                let hole_b = Hole::new(scope, k);

                hole.fill(Type::arrow(
                    Type::new(TypeKind::Hole(hole_a.clone())),
                    Type::new(TypeKind::Hole(hole_e.clone())),
                    Type::new(TypeKind::Hole(hole_b.clone())),
                ));

                Type::sub_hole_type(env.clone(), hole_a, a.clone());
                Type::sub_type_hole(env.clone(), b.clone(), hole_b);
                Type::unify_hole(env, hole_e, e.clone(), true);
            }
            _ => Type::unify_hole(env, hole, ty, true),
        }
    }

    pub fn sub(&self, env: Env, ty: Type) {
        match (self.0.as_ref(), ty.0.as_ref()) {
            (TypeKind::Hole(x), _) if x.is_filled() => x.deref().sub(env, ty),
            (_, TypeKind::Hole(x)) if x.is_filled() => self.sub(env, x.deref()),

            //
            // --------------------------------- <:Exva
            //     Γ [^α] ` ^α <: ^α a Γ [^α]
            //
            (TypeKind::Hole(x), TypeKind::Hole(y)) if x == y => (),

            //
            //     α /∈ FV(A)     Γ [^α] ⊢ ^α :=< A ⊣ ∆
            // ------------------------------------------ <:Instantiate
            //           Γ [^α] ⊢ ^α <: A ⊣ ∆
            //
            (TypeKind::Hole(x), _) => Type::sub_hole_type(env, x.clone(), ty),

            //
            //    α /∈ FV(A)    Γ [^α] ⊢ A =<: ^α ⊣ ∆
            // ---------------------------------------- <:InstantiateR
            //          Γ [^α] ⊢ A <: ^α ⊣ ∆
            //
            (_, TypeKind::Hole(x)) => Type::sub_type_hole(env, self.clone(), x.clone()),

            //
            // Γ, α ⊢ A <: B ⊣ ∆, α, Θ
            // ------------------------- <:∀R
            //    Γ ⊢ A <: ∀α. B ⊣ ∆
            //
            (_, TypeKind::Forall(_, k, t)) => {
                let env = env.add_new_ty(k.clone());
                self.sub(env, t.clone());
            }

            //
            // Γ, ▶^α, ^α ⊢ [^α/α]A <: B ⊣ ∆, ▶^α, Θ
            // ------------------------------------- <:∀L
            //       Γ ⊢ ∀α. A <: B ⊣ ∆ <:∀L
            //
            (TypeKind::Forall(n, k, t), _) => {
                let a_1 = t.instantiate(env.clone(), n.clone(), k.clone());
                a_1.sub(env, ty)
            }

            _ => Type::unify(env, self.clone(), ty),
        }
    }

    pub fn show(&self, env: Env) -> ShowPair {
        ShowPair(env, self.clone())
    }

    pub fn to_list(&self) -> (Vec<(Qualified, Type)>, Option<Hole>) {
        fn to_list(typ: Type, vec: &mut Vec<(Qualified, Type)>) -> Option<Hole> {
            match typ.deref().as_ref() {
                TypeKind::Hole(hole) => Some(hole.clone()),
                TypeKind::RowEmpty => None,
                TypeKind::RowExtend(label, ty, rest) => {
                    vec.push((label.clone(), ty.clone()));
                    to_list(rest.clone(), vec)
                }
                _ => unreachable!(),
            }
        }
        let mut vec = Vec::new();
        let res = to_list(self.clone(), &mut vec);
        (vec, res)
    }
}

/// Structure to be able to show types
pub struct ShowPair(Env, Type);

impl Display for ShowPair {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        self.1.print(self.0.clone(), fmt)
    }
}
