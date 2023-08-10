//! The definition of types for the Vulpi language. It includes a type called [Type] that defines
//! two types: A real and a virtual type. Both are used in the type system, but the virtual type
//! is only used in the type checker as a evaluated state.

pub mod eval;

use std::{cell::RefCell, rc::Rc};

use im_rc::HashSet;
use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract::Qualified;

/// The level of the type. It is used for type checking and type inference.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Level(usize);

/// The state of the type. It's used for diferentiating between the real and virtual type.
pub trait State {
    type Pi;
    type Forall;
    type Hole;
    type Type;
}

/// The type kind is the type of types. It is used for type checking and type inference.
pub enum TypeKind<S: State> {
    /// The type of types
    Type,

    /// The pi type is used for dependent functions.
    Pi(S::Pi),

    /// The forall type is used for polymorphic functions.
    Forall(S::Forall),

    /// The type of holes.
    Hole(S::Hole),

    /// Type for types that are defined by the user.
    Variable(Qualified),

    /// De brujin indexed type.
    Bound(usize),

    /// The type for tuples.
    Tuple(Vec<S::Type>),

    /// The type for type applications
    Application(S::Type, S::Type),

    /// The type for empty rows in effect rows.
    Empty,

    /// The type for extending rows in effect rows.
    Extend(Qualified, S::Type, S::Type),

    /// Hole
    Error,
}

/// The type of types. It is used for type checking and type inference.
#[derive(Clone)]
pub struct Type<S: State>(Rc<TypeKind<S>>);

impl<S: State> Type<S> {
    pub fn new(kind: TypeKind<S>) -> Self {
        Self(Rc::new(kind))
    }
}

impl<S: State> AsRef<TypeKind<S>> for Type<S> {
    fn as_ref(&self) -> &TypeKind<S> {
        &self.0
    }
}

/// The inside of a hole. It contains a Level in the Empty in order to avoid infinite loops and
/// the hole to go out of scope.
pub enum HoleInner<S: State> {
    Empty(Level),
    Row(Level, HashSet<Symbol>),
    Filled(Type<S>),
}

/// A hole is a type that is not yet known. It is used for type inference.
pub struct Hole<S: State>(Rc<RefCell<HoleInner<S>>>);

impl<S: State> Hole<S> {
    pub fn new(hole_inner: HoleInner<S>) -> Self {
        Self(Rc::new(RefCell::new(hole_inner)))
    }

    pub fn row(level: Level, labels: HashSet<Symbol>) -> Self {
        Self(Rc::new(RefCell::new(HoleInner::Row(level, labels))))
    }

    pub fn empty(level: Level) -> Self {
        Self(Rc::new(RefCell::new(HoleInner::Empty(level))))
    }

    pub fn fill(&self, ty: Type<S>) {
        *self.0.borrow_mut() = HoleInner::Filled(ty);
    }
}

pub mod r#virtual {
    use vulpi_intern::Symbol;

    use super::{real::Real, Hole, Level, State, Type};

    /// The virtual state is used as label for the [State] trait as a way to express that the type
    /// contains closures and can be executed.
    #[derive(Clone)]
    pub struct Virtual;

    /// The typing environment is used for type checking and type inference.
    #[derive(Clone)]
    pub struct TypingEnv {
        pub names: im_rc::Vector<Symbol>,
        pub types: im_rc::Vector<Type<Virtual>>,
        pub level: Level,
    }

    pub struct Closure {
        pub env: TypingEnv,
        pub body: Type<Real>,
    }

    /// A pi type without binder. It's used for a bunch of things but not right now :>
    pub struct Pi {
        pub ty: Type<Virtual>,
        pub body: Closure,
    }

    /// A forall with binder so we can bind on types that have higher kinds and ranks.
    pub struct Forall {
        pub name: Symbol,
        pub ty: Type<Virtual>,
        pub body: Closure,
    }

    impl State for Virtual {
        type Pi = Pi;
        type Forall = Forall;
        type Hole = Hole<Virtual>;
        type Type = Type<Virtual>;
    }
}

pub mod real {
    use vulpi_intern::Symbol;

    use super::{Hole, State, Type};

    /// The real state is used as label for the [State] trait as a way to express that the type
    /// contains closures and can be executed.
    #[derive(Clone)]
    pub struct Real;

    /// A pi type without binder. It's used for a bunch of things but not right now :>
    pub struct Pi {
        pub ty: Type<Real>,
        pub body: Type<Real>,
    }

    /// A forall with binder so we can bind on types that have higher kinds and ranks.
    pub struct Forall {
        pub name: Symbol,
        pub ty: Type<Real>,
        pub body: Type<Real>,
    }

    impl State for Real {
        type Pi = Pi;
        type Forall = Forall;
        type Hole = Hole<Real>;
        type Type = Type<Real>;
    }
}
