//! The definition of types for the Vulpi language. It includes a type called [Type] that defines
//! two types: A real and a virtual type. Both are used in the type system, but the virtual type
//! is only used in the type checker as a evaluated state.

pub mod eval;
pub mod unify;

use std::{cell::RefCell, hash::Hash, rc::Rc};

use im_rc::HashSet;
use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract::Qualified;

pub use r#virtual::Env;

use crate::Virtual;

/// The level of the type. It is used for type checking and type inference.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Level(pub usize);

/// The inverse of a the type. It is used for type checking and type inference.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Index(pub usize);

impl Index {
    pub fn shift(self, level: Level) -> Self {
        Self(self.0 + level.0)
    }
}

impl Level {
    /// Increment the level
    pub fn inc(self) -> Self {
        Self(self.0 + 1)
    }

    /// Decrements the level.
    pub fn dec(self) -> Self {
        Self(self.0 - 1)
    }

    /// Transforms a level into an index.
    pub fn to_index(base: Level, current: Level) -> Index {
        if base.0 < current.0 {
            panic!(
                "The base level is {} and the current level is {}",
                base.0, current.0
            )
        }
        Index(base.0 - current.0 - 1)
    }

    pub fn from_index(base: Level, index: Index) -> Level {
        Level(base.0 - index.0 - 1)
    }
}

/// The state of the type. It's used for diferentiating between the real and virtual type.
pub trait State {
    type Pi;
    type Forall;
    type Type;
    type Bound;
}

/// The type kind is the type of types. It is used for type checking and type inference.
pub enum TypeKind<S: State> {
    /// The type of types
    Type,

    /// The type of effects
    Effect,

    /// The type of the rows
    Row,

    /// The pi type is used for dependent functions.
    Arrow(S::Pi),

    /// The forall type is used for polymorphic functions.
    Forall(S::Forall),

    /// The forall type is used for polymorphic functions.
    Exists(S::Forall),

    /// The type of holes.
    Hole(Hole<Virtual>),

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

pub type Effect<S> = Type<S>;

/// A type of a type is the same as a type!
pub type Kind<S> = Type<S>;

impl<S: State> Type<S> {
    pub fn new(kind: TypeKind<S>) -> Self {
        Self(Rc::new(kind))
    }

    /// Checks if the type is a row type.
    pub fn is_row(&self) -> bool {
        matches!(self.0.as_ref(), TypeKind::Row)
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.0.as_ref(), TypeKind::Empty)
    }

    pub(crate) fn forall(forall: S::Forall) -> Self {
        Self::new(TypeKind::Forall(forall))
    }

    pub(crate) fn exists(forall: S::Forall) -> Self {
        Self::new(TypeKind::Exists(forall))
    }

    pub(crate) fn typ() -> Type<S> {
        Type::new(TypeKind::Type)
    }

    pub(crate) fn row() -> Type<S> {
        Type::new(TypeKind::Row)
    }

    pub(crate) fn effect() -> Type<S> {
        Type::new(TypeKind::Effect)
    }

    pub(crate) fn variable(name: Qualified) -> Type<S> {
        Type::new(TypeKind::Variable(name))
    }

    pub(crate) fn error() -> Type<S> {
        Type::new(TypeKind::Error)
    }

    pub(crate) fn bound(level: S::Bound) -> Type<S> {
        Type::new(TypeKind::Bound(level))
    }

    pub(crate) fn tuple(types: Vec<S::Type>) -> Type<S> {
        Type::new(TypeKind::Tuple(types))
    }
}

impl<S: State> AsRef<TypeKind<S>> for Type<S> {
    fn as_ref(&self) -> &TypeKind<S> {
        &self.0
    }
}

/// The inside of a hole. It contains a Level in the Empty in order to avoid infinite loops and
/// the hole to go out of scope.
#[derive(Clone)]
pub enum HoleInner<S: State> {
    Empty(Symbol, Kind<S>, Level),
    Row(Symbol, Level, HashSet<Qualified>),
    Filled(Type<S>),
}

/// A hole is a type that is not yet known. It is used for type inference.
#[derive(Clone)]
pub struct Hole<S: State>(pub Rc<RefCell<HoleInner<S>>>);

impl<S: State> Hash for Hole<S> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let s = (self.0.as_ptr()) as usize;
        s.hash(state);
    }
}

impl<S: State> PartialEq for Hole<S> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<S: State> Eq for Hole<S> {}

impl<S: State> Hole<S> {
    pub fn is_empty(&self) -> bool {
        matches!(&*self.0.borrow(), HoleInner::Empty(_, _, _))
    }

    pub fn is_lacks(&self) -> bool {
        matches!(&*self.0.borrow(), HoleInner::Row(_, _, _))
    }
}

impl<S: State> Hole<S> {
    pub fn new(hole_inner: HoleInner<S>) -> Self {
        Self(Rc::new(RefCell::new(hole_inner)))
    }

    pub fn row(name: Symbol, level: Level, labels: HashSet<Qualified>) -> Self {
        Self(Rc::new(RefCell::new(HoleInner::Row(name, level, labels))))
    }

    pub fn empty(name: Symbol, kind: Kind<S>, level: Level) -> Self {
        Self(Rc::new(RefCell::new(HoleInner::Empty(name, kind, level))))
    }

    pub fn fill(&self, ty: Type<S>) {
        *self.0.borrow_mut() = HoleInner::Filled(ty);
    }
}

pub mod r#virtual {
    use std::{cell::RefCell, collections::HashMap};

    use im_rc::HashSet;
    use vulpi_intern::Symbol;
    use vulpi_location::Span;
    use vulpi_syntax::r#abstract::Qualified;

    use super::{eval::Eval, real::Real, Hole, HoleInner, Kind, Level, State, Type, TypeKind};

    /// The virtual state is used as label for the [State] trait as a way to express that the type
    /// contains closures and can be executed.
    #[derive(Clone)]
    pub struct Virtual;

    /// The typing environment is used for type checking and type inference.
    #[derive(Clone, Default)]
    pub struct Env {
        pub names: im_rc::Vector<Option<Symbol>>,
        pub types: im_rc::Vector<Type<Virtual>>,
        pub kinds: im_rc::Vector<Type<Virtual>>,
        pub vars: im_rc::HashMap<Symbol, Type<Virtual>>,
        pub level: Level,
        pub span: RefCell<Span>,
    }

    impl Env {
        pub fn add_var(&mut self, name: Symbol, ty: Type<Virtual>) {
            self.vars.insert(name, ty);
        }

        /// Sets the location of the environment. It is used for error reporting.
        pub fn on(&self, span: Span) {
            *self.span.borrow_mut() = span;
        }

        pub fn find(&self, name: &Symbol) -> Option<(usize, Type<Virtual>, Type<Virtual>)> {
            self.names
                .iter()
                .zip(self.types.iter())
                .zip(self.kinds.iter())
                .enumerate()
                .find_map(|(i, ((n, t), k))| {
                    if n.as_ref() == Some(name) {
                        Some((i, t.clone(), k.clone()))
                    } else {
                        None
                    }
                })
        }

        /// Adds a type to the environment.
        pub fn add(&self, name: Option<Symbol>, kind: Type<Virtual>) -> Self {
            let mut clone = self.clone();
            clone.names.push_front(name);
            clone.types.push_front(Type::bound(clone.level));
            clone.kinds.push_front(kind);
            clone.level = clone.level.inc();
            clone
        }

        pub fn add_at_end(&self, name: Option<Symbol>, kind: Type<Virtual>) -> Self {
            let mut clone = self.clone();
            clone.names.push_back(name);
            clone.types.push_back(Type::bound(clone.level));
            clone.kinds.push_back(kind);
            clone.level = clone.level.inc();
            clone
        }

        pub fn define(&self, name: Option<Symbol>, ty: Type<Virtual>, kind: Type<Virtual>) -> Self {
            let mut clone = self.clone();
            clone.names.push_front(name);
            clone.types.push_front(ty);
            clone.kinds.push_front(kind);
            clone.level = clone.level.inc();
            clone
        }

        pub fn hole<S: State>(&self, kind: Kind<Virtual>, label: Symbol) -> Type<S> {
            Type::new(TypeKind::Hole(Hole::empty(label, kind, self.level)))
        }

        pub fn lacks<S: State>(&self, symbol: Symbol, hash_set: HashSet<Qualified>) -> Type<S> {
            Type::new(TypeKind::Hole(Hole::row(symbol, self.level, hash_set)))
        }
    }

    /// A simulation of a closure in a type. It contains the environment and the body of the closure.
    pub struct Closure {
        pub env: Env,
        pub body: Type<Real>,
    }

    impl Closure {
        /// "Applies" a closure adding a new type to the environment and evaluating the body.
        pub fn apply(
            &self,
            name: Option<Symbol>,
            arg: Type<Virtual>,
            kind: Type<Virtual>,
        ) -> Type<Virtual> {
            self.body.eval(&self.env.define(name, arg, kind))
        }

        pub fn apply_local(&self, name: Option<Symbol>, arg: Type<Virtual>) -> Type<Virtual> {
            self.body.eval(&self.env.add(name, arg))
        }
    }

    /// A pi type without binder. It's used for a bunch of things but not right now :>
    pub struct Pi {
        pub ty: Type<Virtual>,
        pub effs: Type<Virtual>,
        pub body: Type<Virtual>,
    }

    /// A forall with binder so we can bind on types that have higher kinds and ranks.
    pub struct Forall {
        pub name: Symbol,
        pub kind: Type<Virtual>,
        pub body: Closure,
    }

    impl State for Virtual {
        type Pi = Pi;
        type Forall = Forall;
        type Type = Type<Virtual>;
        type Bound = Level;
    }

    impl Type<Virtual> {
        pub(crate) fn application_spine(&self) -> (Self, Vec<Self>) {
            let mut spine = Vec::new();
            let mut current = self.clone();

            while let TypeKind::Application(left, right) = current.deref().as_ref() {
                spine.push(right.clone());
                current = left.clone();
            }

            spine.reverse();

            (current, spine)
        }

        pub(crate) fn effect_row_set(&self) -> HashMap<Qualified, Type<Virtual>> {
            match self.deref().as_ref() {
                TypeKind::Extend(l, t, rest) => {
                    let mut map = rest.effect_row_set();
                    map.insert(l.clone(), t.clone());
                    map
                }
                _ => Default::default(),
            }
        }

        pub(crate) fn is_row_effect(&self) -> bool {
            match self.deref().as_ref() {
                TypeKind::Extend(_, _, _) => true,
                TypeKind::Empty => true,
                TypeKind::Hole(m) if m.is_lacks() => true,
                _ => false,
            }
        }

        pub fn deref(&self) -> Type<Virtual> {
            match self.as_ref() {
                TypeKind::Hole(h) => match h.0.borrow().clone() {
                    HoleInner::Filled(ty) => ty.deref(),
                    _ => self.clone(),
                },
                _ => self.clone(),
            }
        }

        pub fn application(left: Self, right: Vec<Self>) -> Self {
            right
                .into_iter()
                .fold(left, |acc, x| Type::new(TypeKind::Application(acc, x)))
        }

        pub fn extend(label: Qualified, ty: Type<Virtual>, typ: Type<Virtual>) -> Type<Virtual> {
            Type::new(TypeKind::Extend(label, ty, typ))
        }

        pub(crate) fn row_spine(&self) -> (Option<Hole<Virtual>>, Vec<(Qualified, Self)>) {
            let mut spine = Vec::new();
            let mut current = self.clone();

            while let TypeKind::Extend(label, ty, rest) = current.as_ref() {
                spine.push((label.clone(), ty.clone()));
                current = rest.clone();
            }

            match current.as_ref() {
                TypeKind::Empty => (None, spine),
                TypeKind::Hole(e) => (Some(e.clone()), spine),
                _ => (None, spine),
            }
        }

        pub(crate) fn function(right: Vec<Self>, ret: Self) -> Self {
            right.into_iter().rev().fold(ret, |body, ty| {
                Type::new(TypeKind::Arrow(Pi {
                    ty,
                    effs: Type::new(TypeKind::Empty),
                    body,
                }))
            })
        }
    }
}

pub mod real {
    use std::fmt::Display;

    use crate::Virtual;
    use vulpi_intern::Symbol;
    use vulpi_show::Show as OShow;

    use vulpi_syntax::r#abstract::Qualified;

    use super::{
        eval::Quote, r#virtual::Env, Hole, HoleInner, Index, Level, State, Type, TypeKind,
    };

    /// The real state is used as label for the [State] trait as a way to express that the type
    /// contains closures and can be executed.
    #[derive(Clone)]
    pub struct Real;

    /// A pi type without binder. It's used for a bunch of things but not right now :>
    pub struct Arrow {
        pub ty: Type<Real>,
        pub effs: Type<Real>,
        pub body: Type<Real>,
    }

    /// A forall with binder so we can bind on types that have higher kinds and ranks.
    pub struct Forall {
        pub name: Symbol,
        pub kind: Type<Real>,
        pub body: Type<Real>,
    }

    impl State for Real {
        type Pi = Arrow;
        type Forall = Forall;
        type Type = Type<Real>;
        type Bound = Index;
    }

    /// Environment of names that is useful for pretty printing.
    #[derive(Clone)]
    struct NameEnv(im_rc::Vector<Option<Symbol>>);

    impl From<Env> for NameEnv {
        fn from(env: Env) -> Self {
            Self(env.names)
        }
    }

    impl OShow for Type<Real> {
        fn show(&self) -> vulpi_show::TreeDisplay {
            vulpi_show::TreeDisplay::label("Type")
        }
    }

    impl Type<Real> {
        pub(crate) fn application_spine(&self) -> (Self, Vec<Self>) {
            let mut spine = Vec::new();
            let mut current = self.clone();

            while let TypeKind::Application(left, right) = current.as_ref() {
                spine.push(right.clone());
                current = left.clone();
            }

            spine.reverse();

            (current, spine)
        }

        pub(crate) fn forall_spine(&self) -> (Vec<(Symbol, Self)>, Self) {
            let mut spine = Vec::new();
            let mut current = self.clone();

            while let TypeKind::Forall(Forall { name, kind, body }) = current.as_ref() {
                spine.push((name.clone(), kind.clone()));
                current = body.clone();
            }

            (spine, current)
        }

        pub(crate) fn exists_spine(&self) -> (Vec<(Symbol, Self)>, Self) {
            let mut spine = Vec::new();
            let mut current = self.clone();

            while let TypeKind::Exists(Forall { name, kind, body }) = current.as_ref() {
                spine.push((name.clone(), kind.clone()));
                current = body.clone();
            }

            (spine, current)
        }

        pub fn extend(label: Qualified, ty: Type<Real>, typ: Type<Real>) -> Type<Real> {
            Type::new(TypeKind::Extend(label, ty, typ))
        }

        pub(crate) fn row_spine(&self) -> (Option<Self>, Vec<(Qualified, Self)>) {
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

        pub(crate) fn application(left: Self, right: Vec<Self>) -> Self {
            right
                .into_iter()
                .fold(left, |acc, x| Type::new(TypeKind::Application(acc, x)))
        }

        pub(crate) fn function(right: Vec<Self>, ret: Self) -> Self {
            right.into_iter().rev().fold(ret, |body, ty| {
                Type::new(TypeKind::Arrow(Arrow {
                    ty,
                    effs: Type::new(TypeKind::Empty),
                    body,
                }))
            })
        }
    }

    trait Formattable {
        fn format(&self, env: &NameEnv, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    }

    impl Formattable for Hole<Virtual> {
        fn format(&self, env: &NameEnv, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.0.borrow().clone() {
                HoleInner::Empty(s, _, l) => write!(f, "^{}~{}", s.get(), l.0),
                HoleInner::Row(s, _, _) => write!(f, "~{}", s.get()),
                HoleInner::Filled(forall) => {
                    write!(f, "!")?;
                    forall.quote(Level(env.0.len())).format(env, f)
                }
            }
        }
    }

    impl Formattable for Type<Real> {
        fn format(&self, env: &NameEnv, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.as_ref() {
                TypeKind::Row => write!(f, "Row"),
                TypeKind::Type => write!(f, "Type"),
                TypeKind::Effect => write!(f, "Effect"),
                TypeKind::Arrow(pi) => {
                    write!(f, "(")?;
                    pi.ty.format(env, f)?;
                    write!(f, " -> ")?;

                    match pi.effs.as_ref() {
                        TypeKind::Empty => (),
                        _ => {
                            write!(f, "{{")?;
                            pi.effs.format(env, f)?;
                            write!(f, "}} ")?;
                        }
                    }

                    pi.body.format(env, f)?;
                    write!(f, ")")
                }
                TypeKind::Forall(_) => {
                    let mut env = env.clone();
                    write!(f, "(forall ")?;

                    let (binder, rest) = self.forall_spine();

                    for (i, (name, kind)) in binder.iter().enumerate() {
                        write!(f, "({}: ", name.get())?;
                        kind.format(&env, f)?;
                        write!(f, ")")?;
                        if i != binder.len() - 1 {
                            write!(f, " ")?;
                        }
                        env.0.push_front(Some(name.clone()))
                    }

                    write!(f, ". ")?;

                    rest.format(&env, f)?;

                    write!(f, ")")
                }
                TypeKind::Exists(_) => {
                    let mut env = env.clone();
                    write!(f, "(exists ")?;

                    let (binder, rest) = self.exists_spine();

                    for (i, (name, kind)) in binder.iter().enumerate() {
                        write!(f, "({}: ", name.get())?;
                        kind.format(&env, f)?;
                        write!(f, ")")?;
                        if i != binder.len() - 1 {
                            write!(f, " ")?;
                        }
                        env.0.push_front(Some(name.clone()))
                    }

                    write!(f, ". ")?;

                    rest.format(&env, f)?;

                    write!(f, ")")
                }
                TypeKind::Hole(hole) => hole.format(env, f),
                TypeKind::Variable(n) => write!(f, "{}", n.name.get()),
                TypeKind::Bound(n) => {
                    write!(
                        f,
                        "{}~{}",
                        env.0[n.0]
                            .clone()
                            .unwrap_or(Symbol::intern(&format!("_{}", n.0)))
                            .get(),
                        n.0
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
                TypeKind::Application(_, _) => {
                    let (p, args) = self.application_spine();
                    write!(f, "(")?;
                    p.format(env, f)?;
                    for arg in args {
                        write!(f, " ")?;
                        arg.format(env, f)?;
                    }
                    write!(f, ")")
                }
                TypeKind::Empty => write!(f, "{{}}"),
                TypeKind::Extend(_, _, _) => {
                    let (last, args) = self.row_spine();

                    for (i, (_, e)) in args.iter().enumerate() {
                        e.format(env, f)?;
                        if i != args.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }

                    if let Some(last) = last {
                        write!(f, " | ")?;
                        last.format(env, f)?;
                    }

                    Ok(())
                }
                TypeKind::Error => write!(f, "<ERROR>"),
            }
        }
    }

    impl Type<Real> {
        /// Function that generates a [Show] object responsible for the pretty printing of the type.
        pub fn show(&self, env: &Env) -> Show {
            Show(self.clone(), env.clone().into())
        }
    }

    /// A interface to show types with the correct names.
    pub struct Show(Type<Real>, NameEnv);

    impl Display for Show {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.format(&self.1, f)
        }
    }
}
