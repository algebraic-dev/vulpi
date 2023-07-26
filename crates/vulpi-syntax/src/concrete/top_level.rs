use vulpi_macros::Show;

use crate::tokens::Token;

#[derive(Show)]
pub enum Visibility {
    Public(Token),
    Private,
}

use super::{
    expr::{Expr, PatternArm},
    kind::Kind,
    r#type::{Effects, Type},
    tree::Pattern,
    Lower, Parenthesis, Path, Upper,
};

#[derive(Show)]
pub struct Binder {
    pub left_paren: Token,
    pub pattern: Box<Pattern>,
    pub colon: Token,
    pub typ: Box<Type>,
    pub right_paren: Token,
}

#[derive(Show)]
pub struct LetCase {
    pub pipe: Token,
    pub arm: PatternArm,
}

#[derive(Show)]
pub enum LetMode {
    Body(Token, Box<Expr>),
    Cases(Vec<LetCase>),
}

#[derive(Show)]
pub struct LetDecl {
    pub visibility: Visibility,
    pub let_: Token,
    pub name: Lower,
    pub binders: Vec<Binder>,
    pub ret: Option<(Token, Option<Effects>, Box<Type>)>,
    pub body: LetMode,
}

#[derive(Show)]
pub struct Constructor {
    pub pipe: Token,
    pub name: Upper,
    pub args: Vec<Box<Type>>,
}

#[derive(Show)]
pub struct SumDecl {
    pub constructors: Vec<Constructor>,
}

#[derive(Show)]
pub struct Field {
    pub visibility: Visibility,
    pub name: Lower,
    pub colon: Token,
    pub ty: Box<Type>,
}

#[derive(Show)]
pub struct RecordDecl {
    pub left_brace: Token,
    pub fields: Vec<(Field, Option<Token>)>,
    pub right_brace: Token,
}

#[derive(Show)]
pub struct ExplicitTypeBinder {
    pub name: Lower,
    pub colon: Token,
    pub kind: Box<Kind>,
}

#[derive(Show)]
pub enum TypeBinder {
    Implicit(Lower),
    Explicit(Parenthesis<ExplicitTypeBinder>),
}

#[derive(Show)]
pub enum TypeDef {
    Sum(SumDecl),
    Record(RecordDecl),
    Synonym(Box<Type>),
}

#[derive(Show)]
pub struct TypeDecl {
    pub visibility: Visibility,
    pub type_: Token,
    pub name: Upper,
    pub binders: Vec<TypeBinder>,
    pub def: Option<(Token, TypeDef)>,
}

#[derive(Show)]
pub struct UseAlias {
    pub as_: Token,
    pub alias: Upper,
}

#[derive(Show)]
pub struct UseDecl {
    pub visibility: Visibility,
    pub use_: Token,
    pub path: Path<Upper>,
    pub alias: Option<UseAlias>,
}

#[derive(Show)]
pub struct ModuleInline {
    pub where_: Token,
    pub top_levels: Vec<(TopLevel, Option<Token>)>,
}

#[derive(Show)]
pub struct ModuleDecl {
    pub visibility: Visibility,
    pub mod_: Token,
    pub name: Upper,
    pub part: Option<ModuleInline>,
}

#[derive(Show)]
pub struct EffectField {
    pub name: Lower,
    pub args: Vec<Box<Type>>,
    pub colon: Token,
    pub ret: Box<Type>,
}

#[derive(Show)]
pub struct EffectDecl {
    pub visibility: Visibility,
    pub effect: Token,
    pub name: Upper,
    pub binders: Vec<TypeBinder>,
    pub where_: Token,
    pub fields: Vec<(EffectField, Option<Token>)>,
}

#[derive(Show)]
pub enum TopLevel {
    Let(Box<LetDecl>),
    Type(Box<TypeDecl>),
    Use(Box<UseDecl>),
    Module(Box<ModuleDecl>),
    Effect(Box<EffectDecl>),
    Error(Vec<Token>),
}

#[derive(Show)]
pub struct Program {
    pub top_levels: Vec<TopLevel>,
    pub eof: Token,
}
