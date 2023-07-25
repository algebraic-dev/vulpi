use vulpi_location::Spanned;

use crate::tokens::Token;

use super::{expr::Expr, tree::Pattern};

pub struct LetSttm {
    pub let_: Token,
    pub pattern: Box<Pattern>,
    pub eq: Token,
    pub expr: Box<Expr>,
}

pub enum StatementKind {
    Let(LetSttm),
    Expr(Box<Expr>),
    Error(Vec<Token>),
}

pub type Statement = Spanned<StatementKind>;

pub struct Block {
    pub statements: Vec<(Statement, Option<Token>)>,
}

pub struct DoExpr {
    pub do_: Token,
    pub block: Block,
}
