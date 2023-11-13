//! The concrete syntax tree for the language. This is the output of the parser.

pub mod expr;
pub mod kind;
pub mod literal;
pub mod pattern;
pub mod statements;
pub mod top_level;
pub mod r#type;

use vulpi_intern::Symbol;
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

#[derive(Show, Clone)]
pub struct Upper(pub Token);

impl Upper {
    pub fn symbol(&self) -> Symbol {
        self.0.value.data.clone()
    }
}

#[derive(Show, Clone)]
pub struct Lower(pub Token);

impl Lower {
    pub fn symbol(&self) -> Symbol {
        self.0.value.data.clone()
    }
}

#[derive(Show, Clone)]
pub enum Ident {
    Upper(Upper),
    Lower(Lower),
}

#[derive(Show, Clone)]
pub struct Path<T> {
    pub segments: Vec<(Upper, Token)>,
    pub last: T,
    pub span: Span,
}

impl From<&Path<Upper>> for Vec<Symbol> {
    fn from(value: &Path<Upper>) -> Self {
        value
            .segments
            .iter()
            .map(|(upper, _)| upper.symbol())
            .chain(std::iter::once(value.last.symbol()))
            .collect::<Vec<_>>()
    }
}

impl From<&Path<Lower>> for Vec<Symbol> {
    fn from(value: &Path<Lower>) -> Self {
        value
            .segments
            .iter()
            .map(|(upper, _)| upper.symbol())
            .chain(std::iter::once(value.last.symbol()))
            .collect::<Vec<_>>()
    }
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

#[derive(Show, Clone)]
pub struct Parenthesis<T> {
    pub left: Token,
    pub data: T,
    pub right: Token,
}

impl<T> Parenthesis<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Parenthesis<U> {
        let Parenthesis { left, data, right } = self;

        Parenthesis {
            left,
            data: f(data),
            right,
        }
    }
}
