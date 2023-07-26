use vulpi_location::Spanned;
use vulpi_macros::Show;

use crate::tokens::Token;

use super::{expr::Expr, tree::Pattern};

#[derive(Show)]
pub struct LetSttm {
    pub let_: Token,
    pub pattern: Box<Pattern>,
    pub eq: Token,
    pub expr: Box<Expr>,
}

#[derive(Show)]
pub enum StatementKind {
    Let(LetSttm),
    Expr(Box<Expr>),
    Error(Vec<Token>),
}

pub type Statement = Spanned<StatementKind>;

#[derive(Show)]
pub struct Block {
    pub statements: Vec<(Statement, Option<Token>)>,
}

#[derive(Show)]
pub struct DoExpr {
    pub do_: Token,
    pub block: Block,
}
