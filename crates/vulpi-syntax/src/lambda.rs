use std::{collections::HashMap};

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_macros::Show;

use crate::{r#abstract::Qualified, elaborated::Literal};

#[derive(Show, Clone)]
pub enum ConsDef {
    Enumerated(Qualified, usize),
    Heavy(Qualified, usize, usize),
    NewType,
    Tuple,
}

#[derive(Clone, Show, Debug, PartialEq, Eq, Hash)]
pub enum Case {
    Tuple(usize),
    Constructor(Qualified, usize),
    Literal(Literal),
}

#[derive(Show, Clone)]
pub enum Stmt {
    Let(Symbol, Expr),
    Expr(Expr),
}

#[derive(Show, Clone)]
pub enum Tree {
    Leaf(usize),
    Switch(Expr, Vec<(Case, TagType, Tree)>),
}

#[derive(Show, Clone)]
pub enum TagType {
    Field(usize),
    Number(usize),
    Size,
    None
}

#[derive(Show, Clone)]
pub enum ExprKind {
    Lambda(Vec<Symbol>, Expr),
    Application(Expr, Vec<Expr>),

    Variable(Symbol),
    Constructor(Qualified),
    Function(Qualified),
    Object(usize, Vec<Expr>),

    Projection(Qualified, Expr),
    Access(Expr, usize),

    Block(Vec<Stmt>),
    Literal(Literal),

    RecordInstance(Qualified, Vec<(Symbol, Expr)>),
    RecordUpdate(Qualified, Expr, Vec<(Symbol, Expr)>),

    Tuple(Vec<Expr>),

    Switch(Symbol, Tree, Vec<Expr>),
    
}

pub type Expr = Box<ExprKind>;

#[derive(Show, Clone)]
pub struct LetDecl {
    pub name: Qualified,
    pub body: Expr,
    pub constants: Option<HashMap<Qualified, Span>>
}

#[derive(Show, Clone)]
pub enum TypeDecl {
    Abstract,
    Enum(Vec<(Qualified, usize)>),
    Record(Vec<Qualified>),
}

#[derive(Show, Clone)]
pub struct ExternalDecl {
    pub name: Qualified,
    pub binding: Symbol,
}

#[derive(Show, Clone, Default)]
pub struct Program {
    pub lets: Vec<(Qualified, LetDecl)>,
    pub externals: Vec<(Qualified, Symbol)>,
    pub definitions: HashMap<Qualified, (ConsDef, usize)>,
}