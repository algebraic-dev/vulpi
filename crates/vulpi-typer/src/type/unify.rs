//! Module for unication and subsumption of types.

#![allow(clippy::only_used_in_recursion)]

use im_rc::HashSet;
use vulpi_syntax::r#abstract::Qualified;

use crate::{context::Context, errors::TypeErrorKind};

use super::{
    eval::Quote,
    r#virtual::Pi,
    r#virtual::{Env, Virtual},
    Hole, HoleInner, Level, Type, TypeKind,
};

type Result<T = ()> = std::result::Result<T, TypeErrorKind>;

impl Context {
    pub fn subsumes(&mut self, env: Env, left: Type<Virtual>, right: Type<Virtual>) {
        fn go(ctx: &mut Context, env: Env, left: Type<Virtual>, right: Type<Virtual>) -> Result {
            let l = left.deref();
            let r = right.deref();

            match (left.as_ref(), r.as_ref()) {
                (TypeKind::Hole(n), _) if n.is_empty() => {
                    ctx.sub_hole_type(env, n.clone(), r.clone())
                }
                (_, TypeKind::Hole(n)) if n.is_empty() => {
                    ctx.sub_type_hole(env, l.clone(), n.clone())
                }
                (TypeKind::Arrow(m), TypeKind::Arrow(n)) => {
                    // Change due to variance.
                    go(ctx, env.clone(), n.ty.clone(), m.ty.clone())?;
                    go(ctx, env.clone(), m.effs.clone(), n.effs.clone())?;
                    go(ctx, env, m.body.clone(), n.body.clone())
                }
                (_, TypeKind::Forall(forall)) => {
                    let lvl_ty = Type::new(TypeKind::Bound(env.level));
                    go(
                        ctx,
                        env.add(None, lvl_ty.clone()),
                        l.clone(),
                        forall.body.apply_local(None, lvl_ty),
                    )
                }
                (TypeKind::Forall(_), _) => {
                    let instantiated = ctx.instantiate(&env, &l);
                    go(ctx, env, instantiated, r.clone())
                }
                (_, _) => ctx.unify(env, l, r),
            }
        }

        let result = go(self, env.clone(), left.clone(), right.clone());

        if let Err(kind) = result {
            match kind {
                TypeErrorKind::TypeMismatch(_, _, _) => self.report(
                    &env,
                    TypeErrorKind::TypeMismatch(
                        env.clone(),
                        left.quote(env.level),
                        right.quote(env.level),
                    ),
                ),
                _ => self.report(&env, kind),
            }
        }
    }

    fn sub_hole_type(&mut self, env: Env, left: Hole<Virtual>, right: Type<Virtual>) -> Result {
        match right.deref().as_ref() {
            TypeKind::Forall(forall) => {
                let lvl_ty = Type::new(TypeKind::Bound(env.level));
                self.sub_hole_type(env, left, forall.body.apply_local(None, lvl_ty))
            }
            TypeKind::Arrow(pi) => {
                let HoleInner::Empty(_, kind, _) = left.0.borrow().clone() else { unreachable!() };

                let hole_a = self.hole(&env, kind.clone());
                let hole_b = self.hole(&env, kind);

                left.fill(Type::new(TypeKind::Arrow(Pi {
                    ty: hole_a.clone(),
                    effs: pi.effs.clone(),
                    body: hole_b.clone(),
                })));

                let a = pi.ty.clone();
                let b = pi.body.clone();

                let TypeKind::Hole(hole_a) = hole_a.as_ref() else { unreachable!() };
                let TypeKind::Hole(hole_b) = hole_b.as_ref() else { unreachable!() };

                self.sub_type_hole(env.clone(), a, hole_a.clone())?;
                self.sub_hole_type(env, hole_b.clone(), b)
            }
            _ => self.unify_hole(env, left, right),
        }
    }

    fn sub_type_hole(&mut self, env: Env, left: Type<Virtual>, right: Hole<Virtual>) -> Result {
        let deref = &left.deref();
        match deref.as_ref() {
            TypeKind::Forall(_) => {
                let left = self.instantiate(&env, deref);
                self.sub_type_hole(env, left, right)
            }
            TypeKind::Arrow(pi) => {
                let HoleInner::Empty(_, kind, _) = right.0.borrow().clone() else { unreachable!() };

                let hole_a = self.hole(&env, kind.clone());
                let hole_b = self.hole(&env, kind);

                right.fill(Type::new(TypeKind::Arrow(Pi {
                    ty: hole_a.clone(),
                    effs: pi.effs.clone(),
                    body: hole_b.clone(),
                })));

                let a = pi.ty.clone();
                let b = pi.body.clone();

                let TypeKind::Hole(hole_a) = hole_a.as_ref() else { unreachable!() };
                let TypeKind::Hole(hole_b) = hole_b.as_ref() else { unreachable!() };

                self.sub_hole_type(env.clone(), hole_a.clone(), a)?;
                self.sub_type_hole(env, b, hole_b.clone())
            }
            _ => self.unify_hole(env, right, left),
        }
    }

    fn unify(&mut self, env: Env, left: Type<Virtual>, right: Type<Virtual>) -> Result {
        let l = left.deref();
        let r = right.deref();
        match (l.as_ref(), r.as_ref()) {
            (TypeKind::Tuple(x), TypeKind::Tuple(y)) if x.len() == y.len() => x
                .iter()
                .zip(y.iter())
                .try_for_each(|(x, y)| self.unify(env.clone(), x.clone(), y.clone())),
            (TypeKind::Application(f, a), TypeKind::Application(g, b)) => {
                self.unify(env.clone(), f.clone(), g.clone())?;
                self.unify(env, a.clone(), b.clone())
            }
            (TypeKind::Extend(label, field_ty, row_tail), TypeKind::Extend(_, _, _)) => {
                let (field_ty1, row_tail1) = self.rewrite_row(env.clone(), right, label.clone())?;
                // TODO: Check recursive row types
                self.unify(env.clone(), field_ty.clone(), field_ty1)?;
                self.unify(env, row_tail.clone(), row_tail1)
            }
            (TypeKind::Hole(n), TypeKind::Hole(m)) if n == m => Ok(()),
            (TypeKind::Hole(m), _) => self.unify_hole(env, m.clone(), r),
            (_, TypeKind::Hole(m)) => self.unify_hole(env, m.clone(), l),
            (TypeKind::Bound(x), TypeKind::Bound(y)) if x == y => Ok(()),
            (TypeKind::Empty, TypeKind::Empty) => Ok(()),
            (TypeKind::Type, TypeKind::Type) => Ok(()),
            (TypeKind::Effect, TypeKind::Effect) => Ok(()),
            (TypeKind::Error, _) | (_, TypeKind::Error) => Ok(()),
            (_, _) => Err(TypeErrorKind::TypeMismatch(
                env.clone(),
                left.quote(env.level),
                right.quote(env.level),
            )),
        }
    }

    fn occurs(&self, env: Env, scope: &Level, hole: Hole<Virtual>, ty: Type<Virtual>) -> Result {
        match ty.deref().as_ref() {
            TypeKind::Arrow(pi) => {
                self.occurs(env.clone(), scope, hole.clone(), pi.ty.clone())?;
                self.occurs(env.clone(), scope, hole.clone(), pi.effs.clone())?;
                self.occurs(env, scope, hole, pi.body.clone())
            }
            TypeKind::Forall(forall) => {
                let lvl_ty = Type::new(TypeKind::Bound(env.level));
                self.occurs(env, scope, hole, forall.body.apply_local(None, lvl_ty))
            }
            TypeKind::Hole(h) if h.clone() == hole => Err(TypeErrorKind::InfiniteType),
            TypeKind::Bound(l) if l >= scope => Err(TypeErrorKind::EscapingScope),
            TypeKind::Tuple(t) => t
                .iter()
                .try_for_each(|t| self.occurs(env.clone(), scope, hole.clone(), t.clone())),
            TypeKind::Application(f, a) => {
                self.occurs(env.clone(), scope, hole.clone(), f.clone())?;
                self.occurs(env, scope, hole, a.clone())
            }
            TypeKind::Extend(_, t, p) => {
                self.occurs(env.clone(), scope, hole.clone(), t.clone())?;
                self.occurs(env, scope, hole, p.clone())
            }
            _ => Ok(()),
        }
    }

    fn unify_hole(&mut self, env: Env, hole: Hole<Virtual>, right: Type<Virtual>) -> Result {
        let borrow = hole.0.borrow().clone();
        match borrow {
            HoleInner::Empty(_, _, lvl) => match right.deref().as_ref() {
                TypeKind::Hole(hole1) if hole == hole1.clone() => Ok(()),
                _ => {
                    self.occurs(env, &lvl, hole.clone(), right.clone())?;
                    hole.fill(right);
                    Ok(())
                }
            },
            HoleInner::Row(_, _, ls1) => match right.deref().as_ref() {
                TypeKind::Hole(hole1) if hole == hole1.clone() => Ok(()),
                TypeKind::Hole(hole1) if hole1.is_lacks() => {
                    let HoleInner::Row(_, _, ls) = hole1.0.borrow().clone() else { unreachable!() };
                    let union = ls.union(ls1);

                    let typ = self.lacks(&env, union);
                    hole.fill(typ.clone());
                    hole1.fill(typ);

                    Ok(())
                }
                _ => self.unify_row(env, hole, right),
            },
            HoleInner::Filled(f) => self.unify(env, f, right),
        }
    }

    fn unify_row(&mut self, env: Env, hole: Hole<Virtual>, right: Type<Virtual>) -> Result {
        let HoleInner::Row(_, _, ls) = hole.0.borrow().clone() else { unreachable!() };
        let (mv, ls1) = right.row_spine();
        let ls1 = ls1.into_iter().map(|x| x.0).collect::<HashSet<Qualified>>();

        let binding = ls1.intersection(ls.clone());
        let labels = binding.into_iter().collect::<Vec<_>>();

        if labels.is_empty() {
            if let Some(hole1) = mv {
                let HoleInner::Row(_, _, r1) = hole.0.borrow().clone() else { unreachable!() };
                let c = r1.union(ls);
                let lacks = self.lacks(&env, c);
                hole1.fill(lacks);
            }
            hole.fill(right);
            Ok(())
        } else {
            Err(TypeErrorKind::InvalidLabels(labels))
        }
    }

    fn rewrite_row(
        &mut self,
        env: Env,
        typ: Type<Virtual>,
        n_lab: Qualified,
    ) -> Result<(Type<Virtual>, Type<Virtual>)> {
        match typ.deref().as_ref() {
            TypeKind::Empty => Err(TypeErrorKind::MissingLabel(n_lab)),
            TypeKind::Extend(label, field, res) => {
                if label.clone() == n_lab {
                    return Ok((field.clone(), res.clone()));
                }

                match res.deref().as_ref() {
                    TypeKind::Hole(h) if h.is_lacks() => {
                        let beta = self.lacks(&env, vec![n_lab.clone()].into_iter().collect());
                        let gamma = self.hole(&env, Type::new(TypeKind::Effect));

                        self.unify_row(
                            env,
                            h.clone(),
                            Type::<Virtual>::extend(n_lab, gamma.clone(), beta.clone()),
                        )?;

                        Ok((
                            gamma,
                            Type::<Virtual>::extend(label.clone(), field.clone(), beta),
                        ))
                    }
                    _ => {
                        let (field_ty, rest) = self.rewrite_row(env.clone(), res.clone(), n_lab)?;

                        Ok((
                            field_ty,
                            Type::<Virtual>::extend(label.clone(), field.clone(), rest),
                        ))
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}
