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
    Generalized(Symbol),

    /// A hole is a type that is open to unification.
    Hole(Hole),

    /// A function type that takes a type and returns a type `A -> B`.
    Function(Type, Type),
}

/// A scheme is a type that is polymorphic and binds variables.
#[derive(Debug, Clone)]
pub struct Scheme {
    /// Variables that are bound by this scheme.
    pub variables: Vec<Symbol>,

    /// The target monotype
    pub monotype: Type,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Level(usize);

/// The insides of a hole. When it is empty it carries the level that it was created at. When it is
/// filled it carries the type that it was filled with.
#[derive(Debug, Clone)]
pub enum HoleInner {
    Unbound(Level),
    Link(Type),
}

/// A type that is open to unification
#[derive(Clone, Debug)]
pub struct Hole(Rc<RefCell<HoleInner>>);

impl Hole {
    pub fn new(level: Level) -> Self {
        Self(Rc::new(RefCell::new(HoleInner::Unbound(level))))
    }

    pub fn get(&self) -> HoleInner {
        self.0.borrow().clone()
    }

    pub fn get_mut(&self) -> std::cell::RefMut<'_, HoleInner> {
        self.0.borrow_mut()
    }
}

impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Hole {}

pub trait Types {
    fn free_vars(&self) -> im_rc::HashSet<Symbol>;
}

impl Types for Hole {
    fn free_vars(&self) -> im_rc::HashSet<Symbol> {
        match &*self.0.borrow_mut() {
            HoleInner::Unbound(_) => Default::default(),
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
        }
    }
}

impl Types for Scheme {
    fn free_vars(&self) -> im_rc::HashSet<Symbol> {
        self.monotype.free_vars()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn two_holes_are_the_same() {
        use super::*;

        let hole1 = Hole::new(Level(0));
        let hole2 = Hole::new(Level(0));

        assert_eq!(hole1, hole2);
    }
}
