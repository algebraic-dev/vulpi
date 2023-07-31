//! This is the module responsible for Kinds that are types of types.

use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

use vulpi_intern::Symbol;

/// The kind of a type.
#[derive(Clone)]
pub struct Kind(Rc<KindType>);

impl Kind {
    pub fn new(ty: KindType) -> Self {
        Kind(Rc::new(ty))
    }

    pub fn star() -> Self {
        Kind::new(KindType::Variable(Symbol::intern("*")))
    }

    pub fn arrow(left: Self, right: Self) -> Self {
        Kind::new(KindType::Arrow(left, right))
    }
}

/// The type of level 1 types.
pub enum KindType {
    Variable(Symbol),
    Arrow(Kind, Kind),
    Error,
}

impl Kind {
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
        }
    }

    pub fn unify(&self, other: &Self) {
        match (&*self.0, &*other.0) {
            (KindType::Variable(name), KindType::Variable(other_name)) if name == other_name => (),
            (KindType::Arrow(from1, to1), KindType::Arrow(from2, to2)) => {
                from1.unify(from2);
                to1.unify(to2);
            }
            (KindType::Error, _) => (),
            (_, KindType::Error) => (),
            _ => todo!(),
        }
    }
}

impl Display for Kind {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> std::fmt::Result {
        self.print(fmt)
    }
}
