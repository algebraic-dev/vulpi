use pattern::Pattern;
use tree::{DoExpr, Literal, Type};
use vulpi_location::Spanned;

use crate::tokens::Token;

use super::*;

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

pub struct LambdaExpr {
    pub lambda: Token,
    pub patterns: Vec<Box<Pattern>>,
    pub arrow: Token,
    pub expr: Box<Expr>,
}

pub struct ApplicationExpr {
    pub func: Box<Expr>,
    pub args: Vec<Box<Expr>>,
}

pub struct ProjectionExpr {
    pub expr: Box<Expr>,
    pub dot: Token,
    pub field: Lower,
}

pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: Operator,
    pub right: Box<Expr>,
}

pub struct IfExpr {
    pub if_: Token,
    pub cond: Box<Expr>,
    pub then: Token,
    pub then_expr: Box<Expr>,
    pub else_: Token,
    pub else_expr: Box<Expr>,
}

pub struct PatternArm {
    pub patterns: Vec<Pattern>,
    pub arrow: Token,
    pub expr: Box<Expr>,
    pub guard: Option<(Token, Box<Expr>)>,
}

pub struct WhenExpr {
    pub when: Token,
    pub scrutinee: Box<Expr>,
    pub is: Token,
    pub arms: Vec<PatternArm>,
}

pub struct AnnotationExpr {
    pub expr: Box<Expr>,
    pub colon: Token,
    pub ty: Box<Type>,
}

pub struct LetExpr {
    pub let_: Token,
    pub pattern: Box<Pattern>,
    pub eq: Token,
    pub body: Box<Expr>,
    pub in_: Token,
    pub value: Box<Expr>,
}

pub struct RecordField {
    pub name: Lower,
    pub eq: Token,
    pub expr: Box<Expr>,
}

pub struct RecordInstance {
    pub name: Path<Upper>,
    pub left_brace: Token,
    pub fields: Vec<(RecordField, Option<Token>)>,
    pub right_brace: Token,
}

pub struct RecordUpdate {
    pub left_brace: Token,
    pub expr: Box<Expr>,
    pub with: Token,
    pub fields: Vec<(RecordField, Option<Token>)>,
    pub right_brace: Token,
}

pub enum ExprKind {
    Lambda(LambdaExpr),
    Application(ApplicationExpr),
    Ident(Path<Ident>),
    Acessor(ProjectionExpr),
    Binary(BinaryExpr),
    Let(LetExpr),
    If(IfExpr),
    When(WhenExpr),
    Do(DoExpr),
    Literal(Literal),

    Annotation(AnnotationExpr),
    RecordInstance(RecordInstance),
    RecordUpdate(RecordUpdate),

    Parenthesis(Parenthesis<Box<Expr>>),
}

pub type Expr = Spanned<ExprKind>;
