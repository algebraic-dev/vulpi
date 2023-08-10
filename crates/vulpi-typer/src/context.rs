//! This file declares a mutable environment that is useful to keep track of information that does
//! not need to be immutable like the Env.

use vulpi_intern::Symbol;

use crate::r#type::{
    eval::Quote,
    r#virtual::Env,
    r#virtual::Virtual,
    r#virtual::{Closure, Forall},
    HoleInner, Level, Type, TypeKind,
};

/// A mutable context that is used differently from [Env]. It is used to keep data between every
/// thing inside the type checker.
pub struct Context {
    pub counter: usize,
}

impl Context {
    fn inc_counter(&mut self) -> usize {
        self.counter += 1;
        self.counter - 1
    }

    /// Creates a new name with the prefix `t_` and a unique number.
    pub fn new_name(&mut self) -> Symbol {
        Symbol::intern(&format!("t_{}", self.inc_counter()))
    }

    /// Creates a new hole that is a type that is not yet known
    pub fn hole(&mut self, env: &Env, kind: Type<Virtual>) -> Type<Virtual> {
        env.hole(kind, self.new_name())
    }

    /// Creates a "lacks" hole that stores effects that should lack.
    pub fn lacks(&mut self, env: &Env) -> Type<Virtual> {
        env.lacks(self.new_name())
    }

    /// Instantiates a poly type to a monotype.
    pub fn instantiate(&mut self, env: &Env, ty: &Type<Virtual>) -> Type<Virtual> {
        match ty.deref().as_ref() {
            TypeKind::Forall(forall) => {
                // Determines if a hole should be lack or not checking if it has effect kind.
                let arg = if forall.kind.is_effect() {
                    env.lacks(forall.name.clone())
                } else {
                    env.hole(forall.kind.clone(), forall.name.clone())
                };

                // Applies the body using the hole argument.
                forall.body.apply(Some(forall.name.clone()), arg)
            }
            _ => ty.clone(),
        }
    }

    /// Generalizes a monotype to a poly type.
    pub fn generalize(&mut self, env: &Env, ty: &Type<Virtual>) -> Type<Virtual> {
        fn go(
            ctx: &mut Context,
            env: &Env,
            ty: Type<Virtual>,
            new_vars: &mut Vec<(Symbol, Type<Virtual>)>,
        ) {
            match ty.as_ref() {
                TypeKind::Pi(p) => {
                    go(ctx, env, p.ty.clone(), new_vars);
                    go(ctx, env, p.effs.clone(), new_vars);
                    let arg = Type::new(TypeKind::Bound(env.level));
                    go(
                        ctx,
                        &env.add(None, arg.clone()),
                        p.body.apply(None, arg),
                        new_vars,
                    );
                }
                TypeKind::Forall(forall) => {
                    let arg = Type::new(TypeKind::Bound(env.level));
                    go(ctx, env, forall.kind.clone(), new_vars);
                    go(
                        ctx,
                        &env.add(None, arg.clone()),
                        forall.body.apply(None, arg),
                        new_vars,
                    );
                }
                TypeKind::Hole(hole) => match hole.0.borrow().clone() {
                    HoleInner::Empty(_, k, _) => {
                        let name = ctx.new_name();
                        new_vars.push((name, k));
                        hole.fill(Type::new(TypeKind::Bound(Level(
                            env.level.0 + new_vars.len() - 1,
                        ))));
                    }
                    HoleInner::Row(_, _, _) => (),
                    HoleInner::Filled(filled) => go(ctx, env, filled, new_vars),
                },
                TypeKind::Tuple(t) => {
                    for ty in t.iter() {
                        go(ctx, env, ty.clone(), new_vars);
                    }
                }
                TypeKind::Application(f, a) => {
                    go(ctx, env, f.clone(), new_vars);
                    go(ctx, env, a.clone(), new_vars);
                }
                TypeKind::Extend(_, t, u) => {
                    go(ctx, env, t.clone(), new_vars);
                    go(ctx, env, u.clone(), new_vars);
                }
                TypeKind::Type => (),
                TypeKind::Effect => (),
                TypeKind::Empty => (),
                TypeKind::Bound(_) => (),
                TypeKind::Variable(_) => (),
                TypeKind::Error => (),
            }
        }

        let mut vars = Vec::new();

        go(self, env, ty.clone(), &mut vars);

        vars.iter().fold(ty.clone(), |rest, (name, kind)| {
            Type::forall(Forall {
                name: name.clone(),
                kind: kind.clone(),
                body: Closure {
                    env: env.clone(),
                    body: rest.quote(env.level),
                },
            })
        })
    }
}
