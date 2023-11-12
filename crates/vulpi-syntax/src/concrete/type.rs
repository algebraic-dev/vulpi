use vulpi_location::Spanned;
use vulpi_macros::Show;

use crate::concrete::Lower;
use crate::tokens::Token;

use super::{top_level::TypeBinder, Parenthesis, Path, Upper};

#[derive(Show)]
pub struct TypeArrow {
    pub left: Box<Type>,
    pub arrow: Token,
    pub right: Box<Type>,
}

#[derive(Show)]
pub struct TypeApplication {
    pub func: Box<Type>,
    pub args: Vec<Box<Type>>,
}

#[derive(Show)]
pub struct TypeForall {
    pub forall: Token,
    pub params: Vec<TypeBinder>,
    pub dot: Token,
    pub body: Box<Type>,
}

#[derive(Show)]
pub enum TypeKind {
    Parenthesis(Parenthesis<(Box<Type>, Option<Token>)>),
    Tuple(Parenthesis<Vec<(Box<Type>, Option<Token>)>>),
    Type(Path<Upper>),
    TypeVariable(Lower),
    Arrow(TypeArrow),
    Application(TypeApplication),
    Forall(TypeForall),
    Unit(Token),
}

pub type Type = Spanned<TypeKind>;
