//! This is the module responsible for Kinds that are types of types.

use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    rc::Rc,
};

use vulpi_intern::Symbol;

use crate::env::Env;

/// The kind of a type.
#[derive(Clone)]
pub struct Kind(Rc<KindType>);

impl AsRef<KindType> for Kind {
    fn as_ref(&self) -> &KindType {
        &self.0
    }
}

impl Kind {
    pub fn new(ty: KindType) -> Self {
        Kind(Rc::new(ty))
    }

    pub fn star() -> Self {
        Kind::new(KindType::Variable(Symbol::intern("*")))
    }

    pub fn effect() -> Self {
        Kind::new(KindType::Variable(Symbol::intern("Effect")))
    }

    pub fn var(name: Symbol) -> Self {
        Kind::new(KindType::Variable(name))
    }

    pub fn arrow(left: Self, right: Self) -> Self {
        Kind::new(KindType::Arrow(left, right))
    }
}

/// The type of level 1 types.
pub enum KindType {
    Variable(Symbol),
    Arrow(Kind, Kind),
    Hole(Rc<RefCell<Option<Kind>>>),
    Error,
}

impl Kind {
    pub fn new_hole() -> Self {
        Kind::new(KindType::Hole(Rc::new(RefCell::new(None))))
    }

    pub fn deref(&self) -> Self {
        match &*self.0 {
            KindType::Hole(hole) => match hole.borrow().as_ref() {
                Some(kind) => kind.deref(),
                None => self.clone(),
            },
            _ => self.clone(),
        }
    }

    pub fn print(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.0 {
            KindType::Variable(name) => write!(fmt, "{}", name.get()),
            KindType::Arrow(from, to) => {
                write!(fmt, "(")?;
                from.print(fmt)?;
                write!(fmt, " -> ")?;
                to.print(fmt)?;
                write!(fmt, ")")
            }
            KindType::Error => write!(fmt, "Error"),
            KindType::Hole(hole) => match hole.borrow().as_ref() {
                Some(kind) => kind.print(fmt),
                None => write!(fmt, "_"),
            },
        }
    }

    pub fn unify(&self, env: &Env, other: &Self) {
        match (&*self.0, &*other.0) {
            (KindType::Variable(name), KindType::Variable(other_name)) if name == other_name => (),
            (KindType::Arrow(from1, to1), KindType::Arrow(from2, to2)) => {
                from1.unify(env, from2);
                to1.unify(env, to2);
            }
            (KindType::Hole(n), _) => {
                let kind = n.borrow().clone();
                match kind {
                    Some(kind) => kind.unify(env, other),
                    None => {
                        *n.borrow_mut() = Some(other.clone());
                    }
                }
            }
            (_, KindType::Hole(n)) => {
                let kind = n.borrow().clone();
                match kind {
                    Some(kind) => self.unify(env, &kind),
                    None => {
                        *n.borrow_mut() = Some(self.clone());
                    }
                }
            }
            (KindType::Error, _) => (),
            (_, KindType::Error) => (),
            _ => env.report(crate::error::TypeErrorKind::KindMismatch(
                self.clone(),
                other.clone(),
            )),
        }
    }

    pub fn error() -> Self {
        Kind::new(KindType::Error)
    }
}

impl Display for Kind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> std::fmt::Result {
        self.print(fmt)
    }
}
