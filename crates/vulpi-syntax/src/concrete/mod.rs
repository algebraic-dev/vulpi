//! The concrete syntax tree for the language. This is the output of the parser.

pub mod expr;
pub mod kind;
pub mod literal;
pub mod pattern;
pub mod statements;
pub mod top_level;
pub mod r#type;

use vulpi_macros::Show;

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

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

#[derive(Show)]
pub struct Upper(pub Token);

#[derive(Show)]
pub struct Lower(pub Token);

#[derive(Show)]
pub enum Ident {
    Upper(Upper),
    Lower(Lower),
}

#[derive(Show)]
pub struct Path<T> {
    pub segments: Vec<(Upper, Token)>,
    pub last: T,
    pub span: Span,
}

impl Path<Ident> {
    pub fn diferentiate(self) -> Either<Path<Upper>, Path<Lower>> {
        let Path {
            segments,
            last,
            span,
        } = self;

        let segments = segments
            .into_iter()
            .map(|(upper, dot)| (upper, dot))
            .collect();

        match last {
            Ident::Upper(upper) => Either::Left(Path {
                segments,
                last: upper,
                span,
            }),
            Ident::Lower(lower) => Either::Right(Path {
                segments,
                last: lower,
                span,
            }),
        }
    }
}

#[derive(Show)]
pub struct Parenthesis<T> {
    pub left: Token,
    pub data: T,
    pub right: Token,
}
