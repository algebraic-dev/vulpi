use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_location::Spanned;
use vulpi_macros::Show;

use crate::r#abstract::Qualified;

#[derive(Show)]
pub enum LiteralKind {
    String(Symbol),
    Integer(Symbol),
    Float(Symbol),
    Char(Symbol),
    Unit,
}

pub type Literal = Box<Spanned<LiteralKind>>;

#[derive(Show)]
pub struct LetStatement<T> {
    pub pattern: Pattern,
    pub expr: Expr<T>,
}

#[derive(Show)]
pub enum StatementKind<T> {
    Let(LetStatement<T>),
    Expr(Expr<T>),
    Error,
}

pub type Statement<T> = Spanned<StatementKind<T>>;

pub type Block<T> = Vec<Statement<T>>;

#[derive(Show)]
pub struct PatOr {
    pub left: Pattern,
    pub right: Pattern,
}

#[derive(Show)]
pub struct PatApplication {
    pub func: Qualified,
    pub args: Vec<Pattern>,
}

#[derive(Show)]
pub struct PatEffect {
    pub func: Qualified,
    pub args: Vec<Pattern>,
    pub cont: Option<Symbol>,
}

#[derive(Show)]
pub enum PatternKind {
    Wildcard,
    Variable(Symbol),
    Literal(Literal),
    Or(PatOr),
    Application(PatApplication),

    Error,
}

pub type Pattern = Box<Spanned<PatternKind>>;

#[derive(Show)]
pub struct LambdaExpr<T> {
    pub param: Pattern,
    pub body: Expr<T>,
}

#[derive(Show)]
pub enum AppKind {
    Infix,
    Normal,
}

#[derive(Show)]
pub struct ApplicationExpr<T> {
    pub typ: T,
    pub app: AppKind,
    pub func: Expr<T>,
    pub args: Vec<Expr<T>>,
}

#[derive(Show)]
pub struct ProjectionExpr<T> {
    pub field: Qualified,
    pub expr: Expr<T>,
}

#[derive(Show)]
pub struct PatternArm<T> {
    pub patterns: Vec<Pattern>,
    pub expr: Expr<T>,
    pub guard: Option<Expr<T>>,
}

#[derive(Show)]
pub struct WhenExpr<T> {
    pub scrutinee: Expr<T>,
    pub arms: Vec<PatternArm<T>>,
}

#[derive(Show)]
pub struct LetExpr<T> {
    pub pattern: Pattern,
    pub body: Expr<T>,
    pub value: Expr<T>,
}

#[derive(Show)]
pub struct RecordInstance<T> {
    pub name: Qualified,
    pub fields: Vec<(Symbol, Expr<T>)>,
}

#[derive(Show)]
pub struct RecordUpdate<T> {
    pub expr: Expr<T>,
    pub fields: Vec<(Symbol, Expr<T>)>,
}

#[derive(Show)]
pub struct HandlerExpr<T> {
    pub expr: Expr<T>,
    pub with: Expr<T>,
}

#[derive(Show)]
pub struct CasesExpr<T> {
    pub arms: Vec<PatternArm<T>>,
}

#[derive(Show)]
pub struct Tuple<T> {
    pub exprs: Vec<Expr<T>>,
}

#[derive(Show)]
pub enum ExprKind<T> {
    Lambda(LambdaExpr<T>),
    Application(ApplicationExpr<T>),

    Variable(Symbol),
    Constructor(Qualified),
    Function(Qualified),
    Effect(Qualified),

    Projection(ProjectionExpr<T>),
    Let(LetExpr<T>),
    When(WhenExpr<T>),
    Do(Block<T>),
    Literal(Literal),

    RecordInstance(RecordInstance<T>),
    RecordUpdate(RecordUpdate<T>),
    Handler(HandlerExpr<T>),
    Cases(CasesExpr<T>),
    Tuple(Tuple<T>),

    Error,
}

pub type Expr<T> = Box<Spanned<ExprKind<T>>>;

#[derive(Show)]
pub struct LetCase<T> {
    pub pattern: PatternArm<T>,
}

#[derive(Show)]
pub struct LetMode<T> {
    pub cases: Vec<LetCase<T>>,
}

#[derive(Show)]
pub struct Binder<T> {
    pub pattern: Pattern,
    pub ty: T,
}

#[derive(Show)]
pub struct LetDecl<T> {
    pub binders: Vec<Binder<T>>,
    pub has_effect: bool,
    pub body: Vec<LetCase<T>>,
}

#[derive(Show)]
pub enum TypeDecl {
    Abstract,
    Enum(Vec<Qualified>),
    Record(Vec<Qualified>),
}

pub struct ExternalDecl {
    pub arguments: usize,
}

pub enum Decl {
    Let(LetDecl<Qualified>),
    Type(TypeDecl),
    Effect(Vec<Qualified>),
    External(Qualified, ExternalDecl),
}

#[derive(Default)]
pub struct Program {
    pub decls: HashMap<Qualified, Decl>,
}
