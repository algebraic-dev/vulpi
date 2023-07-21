use std::fmt::Display;
use std::{cell::RefCell, rc::Rc};

use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;

/// A mono type that is reference counted.
pub type Type = Rc<Mono>;

/// A mono type is a type that does not binds any polymorphic variables.
#[derive(Debug)]
pub enum Mono {
    /// A variable is a type that we not that it exists in the context. e.g. `Int`
    Variable(Id<id::Namespace>, Symbol),

    /// A type that is bound to some scheme. e.g. `a`
    Generalized(usize),

    /// A hole is a type that is open to unification.
    Hole(Hole),

    /// A function type that takes a type and returns a type `A -> B`.
    Function(Type, Type),

    /// Application
    Application(Type, Type),

    /// Error type. It's a sentinel value that unifies with everything.
    Error,
}

#[derive(Clone)]
pub enum Kind {
    Star,
    Fun(Box<Kind>, Box<Kind>),
    Error,
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Star => write!(f, "*"),
            Kind::Fun(left, right) => write!(f, "({} -> {})", left, right),
            Kind::Error => write!(f, "ERROR"),
        }
    }
}

impl Mono {
    fn fmt_with_context(
        &self,
        ctx: &[Symbol],
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Mono::Variable(x, symbol) => write!(f, "{}:{}", x.0, symbol.get()),
            Mono::Generalized(n) => write!(f, "{}", ctx[*n].get()),
            Mono::Hole(hole) => hole.get().fmt_with_context(ctx, f),
            Mono::Function(left, right) => {
                write!(f, "(")?;
                left.fmt_with_context(ctx, f)?;
                write!(f, " -> ")?;
                right.fmt_with_context(ctx, f)?;
                write!(f, ")")
            }
            Mono::Application(fun, arg) => {
                write!(f, "(")?;
                fun.fmt_with_context(ctx, f)?;
                write!(f, " ")?;
                arg.fmt_with_context(ctx, f)?;
                write!(f, ")")
            }
            Mono::Error => write!(f, "ERROR"),
        }
    }
}

impl Display for Mono {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_context(&[], f)
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
        self.monotype.fmt_with_context(&self.variables, f)
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
    ) -> std::fmt::Result {
        match self {
            HoleInner::Unbound(n, l) => write!(f, "!{}~{}", n.get(), l.0),
            HoleInner::Link(t) => {
                write!(f, "^")?;
                t.fmt_with_context(ctx, f)
            }
        }
    }
}

impl Display for HoleInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_context(&[], f)
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
            Mono::Generalized(n) => substitute[*n].clone(),
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
