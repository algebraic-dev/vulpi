use crate::tokens::Token;

pub enum Visibility {
    Public(Token),
    Private,
}

use super::{
    expr::{Expr, PatternArm},
    kind::Kind,
    r#type::Type,
    tree::Pattern,
    Lower, Parenthesis, Path, Upper,
};

pub struct Binder {
    pub left_paren: Token,
    pub pattern: Box<Pattern>,
    pub colon: Token,
    pub typ: Box<Type>,
    pub right_paren: Token,
}

pub struct LetCase {
    pub pipe: Token,
    pub arm: PatternArm,
}

pub enum LetMode {
    Body(Token, Box<Expr>),
    Cases(Vec<LetCase>),
}

pub struct LetDecl {
    pub visibility: Visibility,
    pub let_: Token,
    pub name: Lower,
    pub binders: Vec<Binder>,
    pub typ: Option<(Token, Box<Type>)>,
    pub body: LetMode,
}

pub struct Constructor {
    pub pipe: Token,
    pub name: Upper,
    pub args: Vec<Box<Type>>,
}

pub struct SumDecl {
    pub constructors: Vec<Constructor>,
}

pub struct Field {
    pub visibility: Visibility,
    pub name: Lower,
    pub colon: Token,
    pub ty: Box<Type>,
}

pub struct RecordDecl {
    pub left_brace: Token,
    pub fields: Vec<(Field, Option<Token>)>,
    pub right_brace: Token,
}

pub struct ExplicitTypeBinder {
    pub name: Lower,
    pub colon: Token,
    pub kind: Box<Kind>,
}

pub enum TypeBinder {
    Implicit(Token),
    Explicit(Parenthesis<ExplicitTypeBinder>),
}

pub enum TypeDef {
    Sum(SumDecl),
    Record(RecordDecl),
    Synonym(Box<Type>),
}

pub struct TypeDecl {
    pub visibility: Visibility,
    pub type_: Token,
    pub name: Upper,
    pub binders: Vec<TypeBinder>,
    pub eq: Token,
    pub def: TypeDef,
}

pub struct UseAlias {
    pub as_: Token,
    pub alias: Upper,
}

pub struct UseDecl {
    pub visibility: Visibility,
    pub use_: Token,
    pub path: Path<Upper>,
    pub alias: Option<UseAlias>,
}

pub struct ModuleInline {
    pub where_: Token,
    pub top_level: Vec<(TopLevel, Option<Token>)>,
}

pub struct ModuleDecl {
    pub when: Token,
    pub name: Upper,
    pub part: Option<ModuleInline>,
}

pub struct EffectField {
    pub name: Lower,
    pub args: Vec<Box<Type>>,
    pub colon: Token,
    pub ret: Box<Type>,
}

pub struct EffectDecl {
    pub effect: Token,
    pub name: Upper,
    pub binders: Vec<TypeBinder>,
    pub fields: Vec<(EffectField, Option<Token>)>,
}

pub enum TopLevel {
    Let(Box<LetDecl>),
    Type(Box<TypeDecl>),
    Use(Box<UseDecl>),
    Module(Box<ModuleDecl>),
    Error(Vec<Token>),
}

pub struct Program {
    pub top_levels: Vec<TopLevel>,
    pub eof: Token,
}
