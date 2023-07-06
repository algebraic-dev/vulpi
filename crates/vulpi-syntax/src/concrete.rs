//! Concrete syntax tree for the Vulpi language.

use std::ops::Range;

use vulpi_location::{Byte, Spanned};
use vulpi_macros::Tree;

use crate::token::Token as TokenRaw;

type Token = Spanned<TokenRaw>;

#[derive(Debug, Tree)]
pub struct Parenthesis<T> {
    pub left: Token,
    pub data: T,
    pub right: Token,
}

// Paths

#[derive(Debug, Tree)]
pub struct Upper(pub Token);

#[derive(Debug, Tree)]
pub struct Lower(pub Token);

#[derive(Debug, Tree)]
pub struct Ident(pub Token);

#[derive(Debug, Tree)]
pub struct Path<T> {
    pub segments: Vec<(Upper, Token)>,
    pub last: T,
    pub span: Range<Byte>,
}

// Types

#[derive(Debug, Tree)]
pub struct Effects {
    pub left_brace: Token,
    pub effects: Vec<(Box<Type>, Option<Token>)>,
    pub right_brace: Token,
}

#[derive(Debug, Tree)]
pub struct TypeArrow {
    pub left: Box<Type>,
    pub arrow: Token,
    pub effects: Option<Effects>,
    pub right: Box<Type>,
}

#[derive(Debug, Tree)]
pub struct TypeApplication {
    pub left: Box<Type>,
    pub right: Box<Type>,
}

#[derive(Debug, Tree)]
pub struct TypeForall {
    pub forall: Token,
    pub left: Vec<Lower>,
    pub dot: Token,
    pub right: Box<Type>,
}

#[derive(Debug, Tree)]
pub enum TypeKind {
    Parenthesis(Parenthesis<Box<Type>>),
    Upper(Path<Upper>),
    Lower(Lower),
    Arrow(TypeArrow),
    Application(TypeApplication),
    Forall(TypeForall),
    Unit(Token),
}

pub type Type = Spanned<TypeKind>;

// Literals

#[derive(Debug, Tree)]
pub struct LitString(Token);

#[derive(Debug, Tree)]
pub struct LitInteger(Token);

#[derive(Debug, Tree)]
pub struct LitFloat(Token);

#[derive(Debug, Tree)]
pub struct LitChar(Token);

#[derive(Debug, Tree)]
pub struct LitUnit {
    pub left: Token,
    pub right: Token,
}

#[derive(Debug, Tree)]
pub enum LiteralKind {
    String(Token),
    Integer(Token),
    Float(Token),
    Char(Token),
    Unit(Token),
}

pub type Literal = Spanned<LiteralKind>;

// Patterns

#[derive(Debug, Tree)]
pub struct PatAnnotation {
    pub left: Box<Pattern>,
    pub colon: Token,
    pub right: Box<Type>,
}

#[derive(Debug, Tree)]
pub struct PatOr {
    pub left: Box<Pattern>,
    pub pipe: Token,
    pub right: Box<Pattern>,
}

#[derive(Debug, Tree)]
pub struct PatApplication {
    pub func: Path<Upper>,
    pub args: Vec<Box<Pattern>>,
}

#[derive(Debug, Tree)]
pub enum PatternKind {
    Wildcard(Token),
    Upper(Path<Upper>),
    Lower(Path<Lower>),
    Literal(Literal),
    Annotation(PatAnnotation),
    Or(PatOr),
    Application(PatApplication),
    Parenthesis(Parenthesis<Box<Pattern>>),
}

pub type Pattern = Spanned<PatternKind>;

// Block

#[derive(Debug, Tree)]
pub struct LetSttm {
    pub let_: Token,
    pub pattern: Box<Pattern>,
    pub eq: Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Tree)]
pub enum Statement {
    Let(LetSttm),
    Expr(Box<Expr>),
}

#[derive(Debug, Tree)]
pub struct Block {
    pub statements: Vec<(Statement, Option<Token>)>,
}

#[derive(Debug, Tree)]
pub struct DoExpr {
    pub do_: Token,
    pub block: Block,
}

/// Operator

#[derive(Debug, Tree)]
pub enum Operator {
    Add(Token),
    Sub(Token),
    Mul(Token),
    Div(Token),
    Rem(Token),
    And(Token),
    Or(Token),
    Xor(Token),
    Not(Token),
    Eq(Token),
    Neq(Token),
    Lt(Token),
    Gt(Token),
    Le(Token),
    Ge(Token),
    Shl(Token),
    Shr(Token),
    Pipe(Token),
}

#[derive(Debug, Tree)]
pub struct LambdaExpr {
    pub lambda: Token,
    pub pattern: Box<Pattern>,
    pub arrow: Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Tree)]
pub struct ApplicationExpr {
    pub func: Box<Expr>,
    pub args: Vec<Box<Expr>>,
}

#[derive(Debug, Tree)]
pub struct AcessorExpr {
    pub left: Box<Expr>,
    pub dot: Token,
    pub field: Lower,
}

#[derive(Debug, Tree)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: Operator,
    pub right: Box<Expr>,
}

#[derive(Debug, Tree)]
pub struct IfExpr {
    pub if_: Token,
    pub cond: Box<Expr>,
    pub then: Token,
    pub then_expr: Box<Expr>,
    pub else_: Token,
    pub else_expr: Box<Expr>,
}

#[derive(Debug, Tree)]
pub struct WhenCase {
    pub pattern: Box<Pattern>,
    pub arrow: Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Tree)]
pub struct WhenExpr {
    pub when: Token,
    pub scrutinee: Box<Expr>,
    pub is: Token,
    pub cases: Vec<WhenCase>,
}

#[derive(Debug, Tree)]
pub struct AnnotationExpr {
    pub left: Box<Expr>,
    pub colon: Token,
    pub right: Box<Type>,
}

#[derive(Debug, Tree)]
pub struct LetExpr {
    pub let_: Token,
    pub pattern: Box<Pattern>,
    pub eq: Token,
    pub value: Box<Expr>,
    pub in_: Token,
    pub body: Box<Expr>,
}

#[derive(Debug, Tree)]
pub enum ExprKind {
    Lambda(LambdaExpr),
    Application(ApplicationExpr),
    Lower(Path<Lower>),
    Upper(Path<Upper>),
    Acessor(AcessorExpr),
    Binary(BinaryExpr),
    Let(LetExpr),
    If(IfExpr),
    When(WhenExpr),
    Annotation(AnnotationExpr),
    Do(DoExpr),
    Literal(Literal),
    Parenthesis(Parenthesis<Box<Expr>>),
}

pub type Expr = Spanned<ExprKind>;

// Top level

#[derive(Debug, Tree)]
pub struct Binder {
    pub left_paren: Token,
    pub pattern: Box<Pattern>,
    pub colon: Token,
    pub typ: Box<Type>,
    pub right_paren: Token,
}

#[derive(Debug, Tree)]
pub struct LetDecl {
    pub let_: Token,
    pub name: Lower,
    pub binders: Vec<Binder>,
    pub typ: Option<(Token, Box<Type>)>,
    pub eq: Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Tree)]
pub struct Constructor {
    pub pipe: Token,
    pub name: Upper,
    pub args: Vec<Box<Type>>,
}

#[derive(Debug, Tree)]
pub struct SumDecl {
    pub constructors: Vec<Constructor>,
}

#[derive(Debug, Tree)]
pub struct Field {
    pub name: Lower,
    pub colon: Token,
    pub typ: Box<Type>,
}

#[derive(Debug, Tree)]
pub struct RecordDecl {
    pub left_brace: Token,
    pub fields: Vec<Field>,
    pub right_brace: Token,
}

#[derive(Debug, Tree)]
pub enum TypeDef {
    Sum(SumDecl),
    Record(RecordDecl),
    Synonym(Box<Type>),
}

#[derive(Debug, Tree)]
pub struct TypeDecl {
    pub type_: Token,
    pub name: Upper,
    pub binders: Vec<Lower>,
    pub eq: Token,
    pub def: TypeDef,
}

#[derive(Debug, Tree)]
pub struct UseDecl {
    pub use_: Token,
    pub path: Path<Upper>,
}

#[derive(Debug, Tree)]
pub enum TopLevel {
    Let(LetDecl),
    Type(TypeDecl),
    Use(UseDecl),
}
