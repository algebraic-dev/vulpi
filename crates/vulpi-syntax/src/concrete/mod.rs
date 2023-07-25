//! The concrete syntax tree for the language. This is the output of the parser.

pub mod expr;
pub mod kind;
pub mod literal;
pub mod pattern;
pub mod statements;
pub mod top_level;
pub mod r#type;

/// Module that exposes the entire tree
pub mod tree {
    pub use super::expr::*;
    pub use super::kind::*;
    pub use super::literal::*;
    pub use super::pattern::*;
    pub use super::r#type::*;
    pub use super::statements::*;
    pub use super::top_level::*;
}

use vulpi_location::Span;

use crate::tokens::Token;

pub struct Upper(pub Token);
pub struct Lower(pub Token);
pub struct Ident(pub Token);

pub struct Path<T> {
    pub segments: Vec<(Upper, Token)>,
    pub last: T,
    pub span: Span,
}

pub struct Parenthesis<T> {
    pub left: Token,
    pub data: T,
    pub right: Token,
}
