use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_macros::Show;

use crate::r#abstract::Qualified;

#[derive(Show, PartialEq, Eq, Hash, Clone, Debug)]
pub enum LiteralKind {
    String(Symbol),
    Integer(Symbol),
    Float(Symbol),
    Char(Symbol),
    Unit,
}

pub type Literal = Box<LiteralKind>;

#[derive(Show, Clone)]
pub enum Statement {
    Let(Pattern, Expr),
    Expr(Expr),
    Error,
}


#[derive(Show, Clone, Debug)]
pub enum PatternKind {
    Wildcard,
    Variable(Symbol),
    Literal(Literal),
    Constructor(Qualified, Vec<Pattern>),
    Tuple(Vec<Pattern>),

    Error,
}

pub type Pattern = Box<PatternKind>;

#[derive(Show, Clone)]
pub struct PatternArm {
    pub patterns: Vec<Pattern>,
    pub expr: Expr,
    pub guard: Option<Expr>,
}

#[derive(Show, Clone)]
pub enum ExprKind {
    Lambda(Pattern, Expr),
    Application(Expr, Vec<Expr>),

    Variable(Symbol),
    Constructor(Qualified, Qualified),
    Function(Qualified),

    Projection(Qualified, Expr),
    Let(Pattern, Expr, Expr),
    When(Vec<Expr>, Vec<PatternArm>),
    Do(Vec<Statement>),
    Literal(Literal),

    RecordInstance(Qualified, Vec<(Symbol, Expr)>),
    RecordUpdate(Qualified, Expr, Vec<(Symbol, Expr)>),

    Access(Expr, usize),
    Tuple(Vec<Expr>),
}

pub type Expr = Box<ExprKind>;

#[derive(Show)]
pub struct LetDecl {
    pub binders: Vec<Pattern>,
    pub body: Vec<PatternArm>,
}

#[derive(Show)]
pub enum TypeDecl {
    Abstract,
    Enum(Vec<(Qualified, usize)>),
    Record(Vec<Qualified>),
}

#[derive(Show)]
pub struct ExternalDecl {
    pub binding: Symbol,
}

#[derive(Show)]
pub struct Program {
    pub lets: HashMap<Qualified, LetDecl>,
    pub types: HashMap<Qualified, TypeDecl>,
    pub externals: HashMap<Qualified, ExternalDecl>,
}

impl Default for Program {
    fn default() -> Self {
        Self {
            lets: HashMap::new(),
            types: HashMap::new(),
            externals: HashMap::new(),
        }
    }
}
