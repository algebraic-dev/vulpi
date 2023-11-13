//! This file declares a mutable environment that is useful to keep track of information that does
//! not need to be immutable like the Env.

use std::collections::HashMap;

use crate::{
    module::Modules,
    r#type::{eval::Eval, r#virtual::Pi, Hole, State},
};
use vulpi_intern::Symbol;
use vulpi_report::{Diagnostic, Report};
use vulpi_syntax::{elaborated, r#abstract::Qualified};

use crate::{
    errors::{TypeError, TypeErrorKind},
    r#type::{
        eval::Quote,
        r#virtual::Env,
        r#virtual::Virtual,
        real::{self, Real},
        HoleInner, Level, Type, TypeKind,
    },
};

/// A mutable context that is used differently from [Env]. It is used to keep data between every
/// thing inside the type checker.
pub struct Context {
    pub counter: usize,
    pub reporter: Report,
    pub modules: Modules,
    pub elaborated: elaborated::Program<Type<Real>>,
    pub errored: bool,
}

impl Context {
    pub fn new(reporter: Report) -> Self {
        Self {
            counter: 0,
            reporter,
            modules: Default::default(),
            elaborated: Default::default(),
            errored: false,
        }
    }

    pub fn report(&mut self, env: &Env, kind: TypeErrorKind) {
        self.errored = true;
        self.reporter.report(Diagnostic::new(TypeError {
            span: env.span.borrow().clone(),
            kind,
        }));
    }

    fn inc_counter(&mut self) -> usize {
        self.counter += 1;
        self.counter - 1
    }

    pub fn find_prelude_type(&mut self, name: &str, env: Env) -> Type<Virtual> {
        let path = Symbol::intern("Prelude");
        let name = Symbol::intern(name);
        if self.modules.get(&path).types.get(&name).is_some() {
            Type::variable(Qualified { path, name })
        } else {
            self.report(&env, crate::errors::TypeErrorKind::CannotFind(name));
            Type::error()
        }
    }

    /// Creates a new name with the prefix `t_` and a unique number.
    pub fn new_name(&mut self) -> Symbol {
        Symbol::intern(&format!("t_{}", self.inc_counter()))
    }

    /// Creates a new hole that is a type that is not yet known
    pub fn hole<S: State>(&mut self, env: &Env, kind: Type<Virtual>) -> Type<S> {
        env.hole(kind, self.new_name())
    }

    pub fn as_function(
        &mut self,
        env: &Env,
        typ: Type<Virtual>,
    ) -> Option<(Type<Virtual>, Type<Virtual>)> {
        match typ.deref().as_ref() {
            TypeKind::Arrow(pi) => Some((pi.ty.clone(), pi.body.clone())),
            TypeKind::Error => Some((typ.clone(), typ.clone())),
            TypeKind::Forall(_) => {
                let typ = self.instantiate(env, &typ);
                self.as_function(env, typ)
            }
            TypeKind::Hole(empty) => {
                let hole_inner = empty.0.borrow().clone();
                if let HoleInner::Empty(_, kind, _) = hole_inner {
                    let hole_a = self.hole(env, kind.clone());
                    let hole_b = self.hole(env, kind);

                    empty.fill(Type::new(TypeKind::Arrow(Pi {
                        ty: hole_a.clone(),
                        body: hole_b.clone(),
                    })));

                    Some((hole_a, hole_b))
                } else {
                    unreachable!()
                }
            }
            _ => None,
        }
    }

    /// Instantiates a poly type to a monotype.
    pub fn instantiate(&mut self, env: &Env, ty: &Type<Virtual>) -> Type<Virtual> {
        match ty.deref().as_ref() {
            TypeKind::Forall(forall) => {
                let arg = env.hole(forall.kind.clone(), forall.name.clone());
                let kind = forall.kind.clone();
                // Applies the body using the hole argument.
                forall.body.apply(Some(forall.name.clone()), arg, kind)
            }
            _ => ty.clone(),
        }
    }

    pub fn instantiate_with(&mut self, ty: &Type<Virtual>, arg: Type<Virtual>) -> Type<Virtual> {
        match ty.deref().as_ref() {
            TypeKind::Forall(forall) => {
                let kind = forall.kind.clone();
                forall.body.apply(Some(forall.name.clone()), arg, kind)
            }
            _ => ty.clone(),
        }
    }

    pub fn instantiate_with_args(
        &mut self,
        ty: &Type<Virtual>,
        args: Vec<Type<Virtual>>,
    ) -> Type<Virtual> {
        let mut ty = ty.clone();
        for arg in args {
            ty = self.instantiate_with(&ty, arg);
        }
        ty
    }

    pub fn instantiate_all(&mut self, env: &Env, ty: &Type<Virtual>) -> Type<Virtual> {
        match ty.deref().as_ref() {
            TypeKind::Forall(_) => {
                let res = self.instantiate(env, ty);
                self.instantiate_all(env, &res)
            }
            _ => ty.clone(),
        }
    }

    fn accumulate_variables(
        env: Env,
        level: Level,
        ty: Type<Real>,
        new_vars: &mut HashMap<Hole<Virtual>, (Symbol, Level, Type<Real>)>,
        turn: bool,
    ) -> Type<Real> {
        match ty.as_ref() {
            TypeKind::Arrow(p) => {
                let ty =
                    Context::accumulate_variables(env.clone(), level, p.ty.clone(), new_vars, turn);
                let body =
                    Context::accumulate_variables(env, level, p.body.clone(), new_vars, turn);
                Type::new(TypeKind::Arrow(real::Arrow { ty, body }))
            }
            TypeKind::Forall(forall) => {
                let kind = Context::accumulate_variables(
                    env.clone(),
                    level,
                    forall.kind.clone(),
                    new_vars,
                    turn,
                );
                let body =
                    Context::accumulate_variables(env, level, forall.body.clone(), new_vars, turn);
                Type::new(TypeKind::Forall(real::Forall {
                    name: forall.name.clone(),
                    kind,
                    body,
                }))
            }
            TypeKind::Hole(hole) => {
                if new_vars.contains_key(hole) {
                    return ty.clone();
                }

                let l = Level(level.0 + new_vars.len());

                let borrow = hole.0.borrow().clone();
                match borrow {
                    HoleInner::Empty(n, k, _) => {
                        new_vars.insert(hole.clone(), (n, l, k.quote(env.level)));
                        ty.clone()
                    }
                    HoleInner::Filled(e) => {
                        let e = e.quote(env.level);

                        Context::accumulate_variables(env, level, e, new_vars, turn)
                    }
                }
            }
            TypeKind::Tuple(t) => {
                let t = t
                    .iter()
                    .map(|t| {
                        Context::accumulate_variables(env.clone(), level, t.clone(), new_vars, turn)
                    })
                    .collect();
                Type::new(TypeKind::Tuple(t))
            }
            TypeKind::Application(f, a) => {
                let f =
                    Context::accumulate_variables(env.clone(), level, f.clone(), new_vars, turn);
                let a = Context::accumulate_variables(env, level, a.clone(), new_vars, turn);
                Type::new(TypeKind::Application(f, a))
            }
            TypeKind::Bound(n) if turn => {
                let new = (*n).shift(Level(level.0 + new_vars.len()));
                Type::new(TypeKind::Bound(new))
            }
            _ => ty,
        }
    }

    pub fn skolemize(&mut self, env: Env, ty: &Type<Virtual>) -> Type<Virtual> {
        let mut vars = Default::default();

        let real = ty.clone().quote(env.level);

        let real = Context::accumulate_variables(env.clone(), Level(0), real, &mut vars, false);

        let vars = vars.into_iter().collect::<Vec<_>>();

        #[allow(clippy::redundant_clone)]
        let mut new_env = env.clone();

        for (hole, (n, lvl, _)) in vars.iter() {
            let bound = Type::bound(*lvl);
            new_env = new_env.add(Some(n.clone()), bound.clone());
            hole.0.replace(HoleInner::Filled(bound));
        }

        let real = vars.iter().fold(real, |rest, (_, (name, _, kind))| {
            Type::exists(real::Forall {
                name: name.clone(),
                kind: kind.clone(),
                body: rest,
            })
        });

        real.eval(&env)
    }
}
