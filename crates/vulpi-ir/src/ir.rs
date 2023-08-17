use vulpi_intern::Symbol;
use vulpi_macros::Show;
use vulpi_syntax::r#abstract::Qualified;

use crate::pattern::CaseTree;

#[derive(Clone, Show)]
pub struct FnDecl {
    pub name: Qualified,
    pub params: Vec<Symbol>,
    pub body: Expr,
}

#[derive(Clone, Show)]
pub enum Literal {
    String(Symbol),
    Integer(Symbol),
    Float(Symbol),
    Char(Symbol),
}

#[derive(Clone, Show)]
pub enum ExprKind {
    Variable(Symbol),
    Function(Qualified, Vec<Expr>),
    FunPtr(Qualified),

    Lambda(Vec<Symbol>, Expr),
    Application(Expr, Vec<Expr>),

    Projection(Expr, usize),

    Tree(CaseTree, Vec<Expr>),

    Block(Vec<Statement>),

    Literal(Literal),
    Tuple(Vec<Expr>),
    Tag(Qualified, usize),
}

#[derive(Clone, Show)]
pub enum Statement {
    Let(Symbol, Expr),
    Expr(Expr),
}

pub type Expr = Box<ExprKind>;
