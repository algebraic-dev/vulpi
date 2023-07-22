use std::fmt::Display;
use std::{cell::RefCell, rc::Rc};

use im_rc::HashSet;
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;
use vulpi_syntax::resolved;

use crate::context::Env;
use crate::error;

/// A mono type that is reference counted.
pub type Type = Rc<Mono>;

/// A mono type is a type that does not binds any polymorphic variables.
#[derive(Debug)]
pub enum Mono {
    /// A variable is a type that we not that it exists in the context. e.g. `Int`
    Variable(Id<id::Namespace>, Symbol),

    /// A type that is bound to some scheme. e.g. `a`
    Generalized(usize, Symbol),

    /// A hole is a type that is open to unification.
    Hole(Hole),

    /// A function type that takes a type and returns a type `A -> B`.
    Function(Type, Type),

    /// Application
    Application(Type, Type),

    Unit,

    /// Error type. It's a sentinel value that unifies with everything.
    Error,
}

#[derive(Clone, PartialEq, Eq)]
pub enum KindType {
    Star,
    Fun(Kind, Kind),
    Error,
}

impl KindType {
    pub fn is_error(&self) -> bool {
        matches!(self, KindType::Error)
    }
}

pub type Kind = Rc<KindType>;

impl Display for KindType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KindType::Star => write!(f, "*"),
            KindType::Fun(left, right) => write!(f, "({} -> {})", left, right),
            KindType::Error => write!(f, "ERROR"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    None = 0,
    Arrow,
    App,
}

impl Mono {
    fn fmt_with_context(
        &self,
        ctx: &[Symbol],
        f: &mut std::fmt::Formatter<'_>,
        mode: Mode,
    ) -> std::fmt::Result {
        match self {
            Mono::Unit => write!(f, "()"),
            Mono::Variable(_, symbol) => write!(f, "{}", symbol.get()),
            Mono::Generalized(_, s) => write!(f, "{}", s.get()),
            Mono::Hole(hole) => hole.get().fmt_with_context(ctx, f, mode),
            Mono::Function(left, right) => {
                if mode > Mode::None {
                    write!(f, "(")?;
                }

                left.fmt_with_context(ctx, f, Mode::Arrow)?;
                write!(f, " -> ")?;
                right.fmt_with_context(ctx, f, Mode::None)?;

                if mode > Mode::None {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Mono::Application(fun, arg) => {
                if mode == Mode::App {
                    write!(f, "(")?;
                }
                fun.fmt_with_context(ctx, f, Mode::Arrow)?;
                write!(f, " ")?;
                arg.fmt_with_context(ctx, f, Mode::App)?;
                if mode == Mode::App {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Mono::Error => write!(f, "ERROR"),
        }
    }
}

impl Display for Mono {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_context(&[], f, Mode::None)
    }
}
/// A scheme is a type that is polymorphic and binds variables.
#[derive(Debug, Clone)]
pub struct Scheme {
    /// Variables that are bound by this scheme.
    pub variables: Vec<Symbol>,

    /// The target monotype
    pub monotype: Type,
}

impl From<Type> for Scheme {
    fn from(value: Type) -> Self {
        Self {
            variables: vec![],
            monotype: value,
        }
    }
}

impl Scheme {
    pub fn new(variables: Vec<Symbol>, monotype: Type) -> Self {
        Self {
            variables,
            monotype,
        }
    }
}

impl Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "forall ")?;

        for (i, v) in self.variables.iter().enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", v.get())?;
        }

        write!(f, ". ")?;

        self.monotype
            .fmt_with_context(&self.variables, f, Mode::None)
    }
}

/// The De Bruijin Level. This is used to track the level of a hole. When a hole is created it is
/// created at a certain level. When it is filled it is filled with a type that is at the same level
/// or higher.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Level(pub usize);

impl Level {
    pub fn inc(self) -> Self {
        Self(self.0 + 1)
    }

    pub fn dec(self) -> Self {
        Self(self.0 - 1)
    }
}

/// The insides of a hole. When it is empty it carries the level that it was created at. When it is
/// filled it carries the type that it was filled with.
#[derive(Debug, Clone)]
pub enum HoleInner {
    Unbound(Symbol, Level),
    Link(Type),
}

impl HoleInner {
    pub fn fmt_with_context(
        &self,
        ctx: &[Symbol],
        f: &mut std::fmt::Formatter<'_>,
        mode: Mode,
    ) -> std::fmt::Result {
        match self {
            HoleInner::Unbound(n, l) => write!(f, "!{}~{}", n.get(), l.0),
            HoleInner::Link(t) => {
                write!(f, "~")?;
                t.fmt_with_context(ctx, f, mode)
            }
        }
    }
}

impl Display for HoleInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_context(&[], f, Mode::None)
    }
}

/// A type that is open to unification
#[derive(Clone, Debug)]
pub struct Hole(Rc<RefCell<HoleInner>>);

impl Hole {
    pub fn new(name: Symbol, level: Level) -> Self {
        Self(Rc::new(RefCell::new(HoleInner::Unbound(name, level))))
    }

    pub fn get(&self) -> HoleInner {
        self.0.borrow().clone()
    }

    pub fn get_mut(&self) -> std::cell::RefMut<'_, HoleInner> {
        self.0.borrow_mut()
    }

    pub fn fill(&self, t: Type) {
        *self.0.borrow_mut() = HoleInner::Link(t);
    }
}

impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Hole {}

impl Mono {
    pub(crate) fn instantiate_with(self: Type, substitute: &[Type]) -> Type {
        match &&*self {
            Mono::Unit => self.clone(),
            Mono::Generalized(n, _) => substitute[*n].clone(),
            Mono::Hole(hole) => match hole.get() {
                HoleInner::Unbound(_, _) => self.clone(),
                HoleInner::Link(f) => f.instantiate_with(substitute),
            },
            Mono::Function(l, r) => {
                let l = l.clone().instantiate_with(substitute);
                let r = r.clone().instantiate_with(substitute);
                Type::new(Mono::Function(l, r))
            }
            Mono::Error => self.clone(),
            Mono::Variable(_, _) => self.clone(),
            Mono::Application(f, a) => {
                let f = f.clone().instantiate_with(substitute);
                let a = a.clone().instantiate_with(substitute);
                Type::new(Mono::Application(f, a))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn two_holes_are_the_same() {
        use super::*;

        let hole1 = Hole::new(Symbol::intern("a"), Level(0));
        let hole2 = Hole::new(Symbol::intern("a"), Level(0));

        assert_eq!(hole1, hole1.clone());
        assert_ne!(hole1, hole2);
    }
}

pub fn free_vars(env: Env, tree: &resolved::Type) -> HashSet<Symbol> {
    let mut map = HashSet::new();
    free_variables_located(env, tree, &mut map);
    map
}

pub fn free_variables_located(env: Env, tree: &resolved::Type, map: &mut HashSet<Symbol>) {
    env.set_location(tree.range.clone());
    free_variables(env, &tree.data, map);
}

fn free_variables(env: Env, tree: &resolved::TypeKind, map: &mut HashSet<Symbol>) {
    match tree {
        resolved::TypeKind::Upper(_) => (),
        resolved::TypeKind::Lower(l) => {
            map.insert(l.data.clone());
        }
        resolved::TypeKind::Arrow(resolved::TypeArrow { left, right, .. }) => {
            free_variables_located(env.clone(), left, map);
            free_variables_located(env, right, map);
        }
        resolved::TypeKind::Application(resolved::TypeApplication { fun, args }) => {
            free_variables_located(env.clone(), fun, map);
            for arg in args {
                free_variables_located(env.clone(), arg, map);
            }
        }
        resolved::TypeKind::Forall(_) => {
            env.report(error::TypeErrorKind::CannotInferForall);
        }
        resolved::TypeKind::Error => (),
    }
}
