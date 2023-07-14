use std::fmt::Display;
use std::{cell::RefCell, rc::Rc};

use vulpi_storage::interner::Symbol;

/// A mono type that is reference counted.
pub type Type = Rc<Mono>;

/// A mono type is a type that does not binds any polymorphic variables.
#[derive(Debug)]
pub enum Mono {
    /// A variable is a type that we not that it exists in the context. e.g. `Int`
    Variable(Symbol),

    /// A type that is bound to some scheme. e.g. `a`
    Generalized(usize),

    /// A hole is a type that is open to unification.
    Hole(Hole),

    /// A function type that takes a type and returns a type `A -> B`.
    Function(Type, Type),

    /// Error type. It's a sentinel value that unifies with everything.
    Error,
}

impl Mono {
    fn fmt_with_context(
        &self,
        ctx: &[Symbol],
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Mono::Variable(symbol) => write!(f, "{}", symbol.get()),
            Mono::Generalized(n) => write!(f, "{}", ctx[*n].get()),
            Mono::Hole(hole) => hole.get().fmt_with_context(ctx, f),
            Mono::Function(left, right) => write!(f, "({} -> {})", left, right),
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

/// Trait for things that exhibit type like behavior.
pub trait Types {
    /// Returns the free variables in the type.
    fn free_vars(&self) -> im_rc::HashSet<Symbol>;
}

impl Types for Hole {
    fn free_vars(&self) -> im_rc::HashSet<Symbol> {
        match &*self.0.borrow_mut() {
            HoleInner::Unbound(_, _) => Default::default(),
            HoleInner::Link(t) => t.free_vars(),
        }
    }
}

impl Types for Mono {
    fn free_vars(&self) -> im_rc::HashSet<Symbol> {
        match self {
            Mono::Variable(symbol) => {
                let mut set = im_rc::HashSet::new();
                set.insert(symbol.clone());
                set
            }
            Mono::Generalized(_) => Default::default(),
            Mono::Hole(hole) => hole.free_vars(),
            Mono::Function(left, right) => {
                let left = left.free_vars();
                let right = right.free_vars();
                left.union(right)
            }
            Mono::Error => Default::default(),
        }
    }
}

impl Types for Scheme {
    fn free_vars(&self) -> im_rc::HashSet<Symbol> {
        self.monotype.free_vars()
    }
}

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
            Mono::Variable(_) => self.clone(),
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
