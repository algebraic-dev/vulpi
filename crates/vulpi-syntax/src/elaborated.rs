use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_location::{Span, Spanned};
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
pub struct LetStatement<T> {
    pub pattern: Pattern,
    pub expr: Expr<T>,
}

#[derive(Show, Clone)]
pub enum SttmKind<T> {
    Let(LetStatement<T>),
    Expr(Expr<T>),
    Error,
}

pub type Statement<T> = SttmKind<T>;

pub type Block<T> = Vec<Statement<T>>;

#[derive(Show, Clone)]
pub struct PatOr {
    pub left: Pattern,
    pub right: Pattern,
}

#[derive(Show, Clone, Debug)]
pub struct PatApplication {
    pub func: Qualified,
    pub args: Vec<Pattern>,
}

#[derive(Show, Clone, Debug)]
pub enum PatternKind {
    Wildcard,
    Variable(Symbol),
    Literal(Literal),
    Application(PatApplication),
    Tuple(Vec<Pattern>),
    Error,
}

pub type Pattern = Box<PatternKind>;

#[derive(Show, Clone)]
pub struct LambdaExpr<T> {
    pub param: Pattern,
    pub body: Expr<T>,
}

#[derive(Show, Clone)]
pub enum AppKind {
    Infix,
    Normal,
}

#[derive(Show, Clone)]
pub struct ApplicationExpr<T> {
    pub typ: T,
    pub func: Expr<T>,
    pub args: Expr<T>,
}

#[derive(Show, Clone)]
pub struct ProjectionExpr<T> {
    pub field: Qualified,
    pub expr: Expr<T>,
}

#[derive(Show, Clone)]
pub struct PatternArm<T> {
    pub patterns: Vec<Pattern>,
    pub expr: Expr<T>,
    pub guard: Option<Expr<T>>,
}

#[derive(Show, Clone)]
pub struct WhenExpr<T> {
    pub scrutinee: Vec<Expr<T>>,
    pub arms: Vec<PatternArm<T>>,
}

#[derive(Show, Clone)]
pub struct LetExpr<T> {
    pub pattern: Pattern,
    pub body: Expr<T>,
    pub next: Expr<T>,
}

#[derive(Show, Clone)]
pub struct RecordInstance<T> {
    pub name: Qualified,
    pub fields: Vec<(Symbol, Expr<T>)>,
}

#[derive(Show, Clone)]
pub struct RecordUpdate<T> {
    pub name: Qualified,
    pub expr: Expr<T>,
    pub fields: Vec<(Symbol, Expr<T>)>,
}

#[derive(Show, Clone)]
pub struct Tuple<T> {
    pub exprs: Vec<Expr<T>>,
}

#[derive(Show, Clone)]
pub enum ExprKind<T> {
    Lambda(LambdaExpr<T>),
    Application(ApplicationExpr<T>),

    Variable(Symbol),
    Constructor(Qualified, Qualified),
    Function(Qualified, T),

    Projection(ProjectionExpr<T>),
    Let(LetExpr<T>),
    When(WhenExpr<T>),
    Do(Block<T>),
    Literal(Literal),

    RecordInstance(RecordInstance<T>),
    RecordUpdate(RecordUpdate<T>),
    Tuple(Tuple<T>),

    Error,
}

pub type Expr<T> = Spanned<Box<ExprKind<T>>>;

#[derive(Show, Clone)]
pub struct LetDecl<T> {
    pub name: Qualified,
    pub binders: Vec<(Pattern, T)>,
    pub body: Vec<PatternArm<T>>,
    pub constants: Option<HashMap<Qualified, Span>>,
}

#[derive(Show, Clone)]
pub enum TypeDecl {
    Abstract,
    Enum(Vec<(Qualified, usize)>),
    Record(Vec<Qualified>),
}

#[derive(Show, Clone)]
pub struct ExternalDecl<T> {
    pub name: Qualified,
    pub typ: T,
    pub binding: Symbol,
}

#[derive(Show, Clone)]
pub struct Program<T> {
    pub modules: HashMap<Symbol, Program<T>>,
    pub lets: HashMap<Qualified, LetDecl<T>>,
    pub types: HashMap<Qualified, TypeDecl>,
    pub externals: HashMap<Qualified, ExternalDecl<T>>,
}

impl<T> Default for Program<T> {
    fn default() -> Self {
        Self {
            modules: HashMap::new(),
            lets: HashMap::new(),
            types: HashMap::new(),
            externals: HashMap::new(),
        }
    }
}
