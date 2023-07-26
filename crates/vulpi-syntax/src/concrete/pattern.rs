use vulpi_location::Spanned;

use crate::tokens::Token;

use super::{literal::Literal, r#type::Type, Lower, Parenthesis, Path, Upper};

pub struct PatAscription {
    pub left: Box<Pattern>,
    pub colon: Token,
    pub right: Box<Type>,
}

pub struct PatOr {
    pub left: Box<Pattern>,
    pub pipe: Token,
    pub right: Box<Pattern>,
}

pub struct PatApplication {
    pub func: Path<Upper>,
    pub args: Vec<Box<Pattern>>,
}

pub enum PatternKind {
    Wildcard(Token),
    Constructor(Path<Upper>),
    Effect(Path<Lower>),
    Variable(Lower),
    Literal(Literal),
    Annotation(PatAscription),
    Or(PatOr),
    Application(PatApplication),
    Parenthesis(Parenthesis<Box<Pattern>>),
}

pub type Pattern = Spanned<PatternKind>;
