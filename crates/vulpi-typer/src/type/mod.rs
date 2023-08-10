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

/// The inverse of a the type. It is used for type checking and type inference.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Index(usize);

impl Level {
    pub fn inc(self) -> Self {
        Self(self.0 + 1)
    }

    pub fn dec(self) -> Self {
        Self(self.0 - 1)
    }

    pub fn to_index(base: Level, current: Level) -> Index {
        Index(base.0 - current.0 - 1)
    }
}

/// The state of the type. It's used for diferentiating between the real and virtual type.
pub trait State {
    type Pi;
    type Forall;
    type Hole;
    type Type;
    type Bound;
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
    Bound(S::Bound),

    /// The type for tuples.
    Tuple(Vec<S::Type>),

    /// The type for type applications
    Application(S::Type, S::Type),

    /// The type for empty rows in effect rows.
    Empty,

    /// The type for extending rows in effect rows.
    Extend(Qualified, S::Type, S::Type),

    /// A type error.
    Error,
}

/// The type of types. It is used for type checking and type inference.
#[derive(Clone)]
pub struct Type<S: State>(Rc<TypeKind<S>>);

/// A type of a type is the same as a type!
pub type Kind<S> = Type<S>;

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
    use vulpi_syntax::r#abstract::Qualified;

    use super::{eval::Eval, real::Real, Hole, Level, State, Type, TypeKind};

    /// The virtual state is used as label for the [State] trait as a way to express that the type
    /// contains closures and can be executed.
    #[derive(Clone)]
    pub struct Virtual;

    /// The typing environment is used for type checking and type inference.
    #[derive(Clone)]
    pub struct Env {
        pub names: im_rc::Vector<Option<Symbol>>,
        pub types: im_rc::Vector<Type<Virtual>>,
        pub level: Level,
    }

    impl Env {
        /// Adds a type to the environment.
        pub fn add(&self, name: Option<Symbol>, ty: Type<Virtual>) -> Self {
            let mut clone = self.clone();
            clone.names.push_front(name);
            clone.types.push_front(ty);
            clone.level = clone.level.inc();
            clone
        }
    }

    /// A simulation of a closure in a type. It contains the environment and the body of the closure.
    pub struct Closure {
        pub env: Env,
        pub body: Type<Real>,
    }

    impl Closure {
        /// "Applies" a closure adding a new type to the environment and evaluating the body.
        pub fn apply(&self, name: Option<Symbol>, arg: Type<Virtual>) -> Type<Virtual> {
            self.body.eval(&self.env.add(name, arg))
        }
    }

    /// A pi type without binder. It's used for a bunch of things but not right now :>
    pub struct Pi {
        pub ty: Type<Virtual>,
        pub effs: Type<Virtual>,
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
        type Bound = Level;
    }

    impl Type<Virtual> {
        pub fn application_spine(&self) -> (Self, Vec<Self>) {
            let mut spine = Vec::new();
            let mut current = self.clone();

            while let TypeKind::Application(left, right) = current.as_ref() {
                spine.push(right.clone());
                current = left.clone();
            }

            spine.reverse();

            (current, spine)
        }

        pub fn row_spine(&self) -> (Option<Self>, Vec<(Qualified, Self)>) {
            let mut spine = Vec::new();
            let mut current = self.clone();

            while let TypeKind::Extend(label, ty, rest) = current.as_ref() {
                spine.push((label.clone(), ty.clone()));
                current = rest.clone();
            }

            match current.as_ref() {
                TypeKind::Empty => (None, spine),
                _ => (Some(current), spine),
            }
        }
    }
}

pub mod real {
    use vulpi_intern::Symbol;

    use super::{r#virtual::Env, Hole, HoleInner, Index, State, Type, TypeKind};

    /// The real state is used as label for the [State] trait as a way to express that the type
    /// contains closures and can be executed.
    #[derive(Clone)]
    pub struct Real;

    /// A pi type without binder. It's used for a bunch of things but not right now :>
    pub struct Pi {
        pub ty: Type<Real>,
        pub effs: Type<Real>,
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
        type Bound = Index;
    }

    #[derive(Clone)]
    struct NameEnv(im_rc::Vector<Option<Symbol>>);

    impl From<Env> for NameEnv {
        fn from(env: Env) -> Self {
            Self(env.names.clone())
        }
    }

    impl NameEnv {
        fn add(&self, name: Option<Symbol>) -> Self {
            let mut clone = self.clone();
            clone.0.push_front(name);
            clone
        }
    }
    trait Formattable {
        fn format(&self, env: &NameEnv, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    }

    impl Formattable for Hole<Real> {
        fn format(&self, env: &NameEnv, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &*self.0.borrow() {
                HoleInner::Empty(e) => write!(f, "^{}", e.0),
                HoleInner::Row(e, s) => write!(f, "~{}", e.0),
                HoleInner::Filled(forall) => forall.format(env, f),
            }
        }
    }

    impl Formattable for Type<Real> {
        fn format(&self, env: &NameEnv, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.as_ref() {
                TypeKind::Type => write!(f, "Type"),
                TypeKind::Pi(pi) => {
                    write!(f, "(")?;
                    pi.ty.format(env, f)?;
                    write!(f, " -> ")?;
                    pi.body.format(&env.add(None), f)?;
                    write!(f, ")")
                }
                TypeKind::Forall(forall) => {
                    write!(f, "forall ")?;
                    write!(f, "({}", forall.name.get())?;
                    write!(f, " : ")?;
                    forall.ty.format(env, f)?;
                    write!(f, ") . ")?;
                    forall.body.format(&env.add(Some(forall.name.clone())), f)?;
                    write!(f, "")
                }

                TypeKind::Hole(hole) => hole.format(env, f),
                TypeKind::Variable(n) => write!(f, "{}", n.name.get()),
                TypeKind::Bound(n) => {
                    write!(
                        f,
                        "{}",
                        env.0[n.0].clone().unwrap_or(Symbol::intern("_")).get()
                    )
                }
                TypeKind::Tuple(t) => {
                    write!(f, "(")?;
                    for (i, ty) in t.iter().enumerate() {
                        ty.format(env, f)?;
                        if i != t.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")
                }
                TypeKind::Application(p, a) => {
                    p.format(env, f)?;
                    write!(f, " ")?;
                    a.format(env, f)
                }
                TypeKind::Empty => todo!(),
                TypeKind::Extend(_, _, _) => todo!(),
                TypeKind::Error => todo!(),
            }
        }
    }
}
