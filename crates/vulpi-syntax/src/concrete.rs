//! Wrappers for the parse tree. This is the main interface for the parser. It's flexible enough to
//! handle a lot of errors and be resilient in a certain way.

use vulpi_macros::newtype;

use crate::tree::{Specialized, Tree, TreeKind::*};

#[newtype(TypeForall)]
pub struct ForallNode<'a>(pub &'a Tree<'a>);

impl<'a> ForallNode<'a> {
    pub fn args(&'a self) -> Option<Vec<String>> {
        self.find("args")?.traverse(|arg| arg.lower_id())
    }

    pub fn body(&'a self) -> Option<TypeNode<'a>> {
        self.find("body")?.to::<TypeNode>()
    }
}

#[newtype(TypeArrow)]
#[derive(Debug)]
pub struct TypeArrowNode<'a>(pub &'a Tree<'a>);

impl<'a> TypeArrowNode<'a> {
    pub fn left(&'a self) -> Option<TypeNode<'a>> {
        self.find("left")?.to::<TypeNode>()
    }

    pub fn right(&'a self) -> Option<TypeNode<'a>> {
        self.find("right")?.to::<TypeNode>()
    }
}

#[newtype(TypeApplication)]
#[derive(Debug)]
pub struct TypeApplicationNode<'a>(pub &'a Tree<'a>);

impl<'a> TypeApplicationNode<'a> {
    pub fn left(&'a self) -> Option<TypeNode<'a>> {
        self.at(0)?.to::<TypeNode>()
    }

    pub fn right(&'a self) -> Option<TypeNode<'a>> {
        self.at(1)?.to::<TypeNode>()
    }
}

#[newtype(TypeId)]
#[derive(Debug)]
pub struct TypeIdNode<'a>(pub &'a Tree<'a>);

impl<'a> TypeIdNode<'a> {
    pub fn id(&'a self) -> Option<String> {
        self.at(0)?.lower_id()
    }
}

#[newtype(TypePoly)]
pub struct TypePolyNode<'a>(pub &'a Tree<'a>);

impl<'a> TypePolyNode<'a> {
    pub fn id(&'a self) -> Option<String> {
        self.at(0)?.lower_id()
    }

    pub fn body(&'a self) -> Option<TypeNode<'a>> {
        self.at(1)?.to::<TypeNode>()
    }
}

pub enum TypeEnum<'a> {
    Forall(ForallNode<'a>),
    Arrow(TypeArrowNode<'a>),
    Application(TypeApplicationNode<'a>),
    Id(TypeIdNode<'a>),
    Poly(TypePolyNode<'a>),
}

#[newtype(Type)]
#[derive(Debug)]
pub struct TypeNode<'a>(pub &'a Tree<'a>);

impl<'a> TypeNode<'a> {
    pub fn forall(&'a self) -> Option<ForallNode<'a>> {
        self.at(0)?.to::<ForallNode>()
    }

    pub fn type_application(&'a self) -> Option<TypeApplicationNode<'a>> {
        self.at(0)?.to::<TypeApplicationNode>()
    }

    pub fn type_arrow(&'a self) -> Option<TypeArrowNode<'a>> {
        self.at(0)?.to::<TypeArrowNode>()
    }

    pub fn type_id(&'a self) -> Option<TypeIdNode<'a>> {
        self.at(0)?.to::<TypeIdNode>()
    }

    pub fn type_poly(&'a self) -> Option<TypePolyNode<'a>> {
        self.at(0)?.to::<TypePolyNode>()
    }

    pub fn to_enum(&'a self) -> Option<TypeEnum<'a>> {
        match self.at(0)?.kind()? {
            TypeForall => self.forall().map(TypeEnum::Forall),
            TypeArrow => self.type_arrow().map(TypeEnum::Arrow),
            TypeApplication => self.type_application().map(TypeEnum::Application),
            TypeId => self.type_id().map(TypeEnum::Id),
            TypePoly => self.type_poly().map(TypeEnum::Poly),
            _ => None,
        }
    }
}
