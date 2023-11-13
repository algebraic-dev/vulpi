use pattern::Pattern;
use tree::{DoExpr, Literal, Type};
use vulpi_location::Spanned;
use vulpi_macros::Show;

use crate::tokens::Token;

use super::*;

#[derive(Show, Clone)]
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

#[derive(Show, Clone)]
pub struct LambdaExpr {
    pub lambda: Token,
    pub patterns: Vec<Box<Pattern>>,
    pub arrow: Token,
    pub expr: Box<Expr>,
}

#[derive(Show, Clone)]
pub struct ApplicationExpr {
    pub func: Box<Expr>,
    pub args: Vec<Box<Expr>>,
}

#[derive(Show, Clone)]
pub struct ProjectionExpr {
    pub expr: Box<Expr>,
    pub dot: Token,
    pub field: Lower,
}

#[derive(Show, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: Operator,
    pub right: Box<Expr>,
}

#[derive(Show, Clone)]
pub struct IfExpr {
    pub if_: Token,
    pub cond: Box<Expr>,
    pub then: Token,
    pub then_expr: Box<Expr>,
    pub else_: Token,
    pub else_expr: Box<Expr>,
}

#[derive(Show, Clone)]
pub struct PatternArm {
    pub patterns: Vec<(Box<Pattern>, Option<Token>)>,
    pub arrow: Token,
    pub expr: Box<Expr>,
    pub guard: Option<(Token, Box<Expr>)>,
}

#[derive(Show, Clone)]
pub struct WhenExpr {
    pub when: Token,
    pub scrutinee: Vec<(Box<Expr>, Option<Token>)>,
    pub is: Token,
    pub arms: Vec<PatternArm>,
}

#[derive(Show, Clone)]
pub struct AnnotationExpr {
    pub expr: Box<Expr>,
    pub colon: Token,
    pub ty: Box<Type>,
}

#[derive(Show, Clone)]
pub struct LetExpr {
    pub let_: Token,
    pub pattern: Box<Pattern>,
    pub eq: Token,
    pub body: Box<Expr>,
    pub in_: Token,
    pub value: Box<Expr>,
}

#[derive(Show, Clone)]
pub struct RecordField {
    pub name: Lower,
    pub eq: Token,
    pub expr: Box<Expr>,
}

#[derive(Show, Clone)]
pub struct RecordInstance {
    pub name: Path<Upper>,
    pub left_brace: Token,
    pub fields: Vec<(RecordField, Option<Token>)>,
    pub right_brace: Token,
}

#[derive(Show, Clone)]
pub struct RecordUpdate {
    pub expr: Box<Expr>,
    pub left_brace: Token,
    pub fields: Vec<(RecordField, Option<Token>)>,
    pub right_brace: Token,
}

pub type Tuple = Parenthesis<Vec<(Box<Spanned<ExprKind>>, Option<Token>)>>;

#[derive(Show, Clone)]
pub enum ExprKind {
    Lambda(LambdaExpr),
    Application(ApplicationExpr),

    Variable(Lower),
    Constructor(Path<Upper>),
    Function(Path<Lower>),

    Projection(ProjectionExpr),
    Binary(BinaryExpr),
    Let(LetExpr),
    When(WhenExpr),
    Do(DoExpr),
    Literal(Literal),

    Annotation(AnnotationExpr),
    RecordInstance(RecordInstance),
    RecordUpdate(RecordUpdate),

    Parenthesis(Parenthesis<(Box<Spanned<ExprKind>>, Option<Token>)>),
    Tuple(Tuple),
}

pub type Expr = Spanned<ExprKind>;
