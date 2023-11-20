//! This file declares a mutable environment that is useful to keep track of information that does
//! not need to be immutable like the Env.

use vulpi_intern::Symbol;
use vulpi_report::{Diagnostic, Report};
use vulpi_syntax::{elaborated, r#abstract::Qualified};

use crate::{
    errors::{TypeError, TypeErrorKind},
    module::Modules,
    r#virtual::Env,
    r#virtual::Pi,
    r#virtual::Virtual,
    real::Real,
    HoleInner, State, Type, TypeKind,
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
            TypeKind::Arrow(pi) => Some((pi.typ.clone(), pi.body.clone())),
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
                        typ: hole_a.clone(),
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
    pub fn instantiate(&mut self, env: &Env, typ: &Type<Virtual>) -> Type<Virtual> {
        match typ.deref().as_ref() {
            TypeKind::Forall(forall) => {
                let arg = env.hole(forall.kind.clone(), forall.name.clone());
                let kind = forall.kind.clone();
                // Applies the body using the hole argument.
                forall.body.apply(Some(forall.name.clone()), arg, kind)
            }
            _ => typ.clone(),
        }
    }

    pub fn instantiate_with(&mut self, typ: &Type<Virtual>, arg: Type<Virtual>) -> Type<Virtual> {
        match typ.deref().as_ref() {
            TypeKind::Forall(forall) => {
                let kind = forall.kind.clone();
                forall.body.apply(Some(forall.name.clone()), arg, kind)
            }
            _ => typ.clone(),
        }
    }

    pub fn instantiate_with_arguments(
        &mut self,
        ty: &Type<Virtual>,
        args: Vec<Type<Virtual>>,
    ) -> Type<Virtual> {
        let mut typ = ty.clone();
        for arg in args {
            typ = self.instantiate_with(&typ, arg);
        }
        typ
    }

    pub fn instantiate_all(&mut self, env: &Env, typ: &Type<Virtual>) -> Type<Virtual> {
        match typ.deref().as_ref() {
            TypeKind::Forall(_) => {
                let res = self.instantiate(env, typ);
                self.instantiate_all(env, &res)
            }
            _ => typ.clone(),
        }
    }
}
