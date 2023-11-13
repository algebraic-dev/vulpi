use vulpi_location::Spanned;
use vulpi_macros::Show;

use crate::tokens::Token;

use super::{expr::Expr, tree::Pattern};

#[derive(Show, Clone)]
pub struct LetSttm {
    pub let_: Token,
    pub pattern: Box<Pattern>,
    pub eq: Token,
    pub expr: Box<Expr>,
}

#[derive(Show, Clone)]
pub enum StatementKind {
    Let(LetSttm),
    Expr(Box<Expr>),
    Error(Vec<Token>),
}

pub type Sttm = Spanned<StatementKind>;

#[derive(Show, Clone)]
pub struct Block {
    pub statements: Vec<Sttm>,
}

#[derive(Show, Clone)]
pub struct DoExpr {
    pub do_: Token,
    pub block: Block,
}
