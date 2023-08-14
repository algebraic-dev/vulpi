use vulpi_macros::Show;

use crate::tokens::Token;

#[derive(Show, Clone)]
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
    pub typ: Option<(Token, Box<Type>)>,
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
    pub top_levels: Vec<TopLevel>,
}

impl ModuleInline {
    pub fn modules(&self) -> impl Iterator<Item = &ModuleDecl> {
        self.top_levels
            .iter()
            .filter_map(|top_level| match &top_level {
                TopLevel::Module(module) => Some(&**module),
                _ => None,
            })
    }

    pub fn uses(&self) -> impl Iterator<Item = &UseDecl> {
        self.top_levels
            .iter()
            .filter_map(|top_level| match &top_level {
                TopLevel::Use(use_) => Some(&**use_),
                _ => None,
            })
    }
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
    pub visibility: Visibility,
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
    pub where_: Option<Token>,
    pub fields: Vec<EffectField>,
}

#[derive(Show)]
pub struct ExternalDecl {
    pub visibility: Visibility,
    pub external: Token,
    pub name: Lower,
    pub colon: Token,
    pub typ: Box<Type>,
    pub equal: Token,
    pub str: Token,
}

#[derive(Show)]
pub enum TopLevel {
    Let(Box<LetDecl>),
    Type(Box<TypeDecl>),
    Use(Box<UseDecl>),
    Module(Box<ModuleDecl>),
    Effect(Box<EffectDecl>),
    Error(Vec<Token>),
    External(Box<ExternalDecl>),
}

#[derive(Show)]
pub struct Program {
    pub top_levels: Vec<TopLevel>,
    pub eof: Token,
}

impl Program {
    pub fn modules(&self) -> impl Iterator<Item = &ModuleDecl> {
        self.top_levels
            .iter()
            .filter_map(|top_level| match top_level {
                TopLevel::Module(module) => Some(&**module),
                _ => None,
            })
    }

    pub fn uses(&self) -> impl Iterator<Item = &UseDecl> {
        self.top_levels
            .iter()
            .filter_map(|top_level| match top_level {
                TopLevel::Use(use_) => Some(&**use_),
                _ => None,
            })
    }

    pub fn types(&self) -> impl Iterator<Item = &TypeDecl> {
        self.top_levels
            .iter()
            .filter_map(|top_level| match top_level {
                TopLevel::Type(type_) => Some(&**type_),
                _ => None,
            })
    }

    pub fn lets(&self) -> impl Iterator<Item = &LetDecl> {
        self.top_levels
            .iter()
            .filter_map(|top_level| match top_level {
                TopLevel::Let(let_) => Some(&**let_),
                _ => None,
            })
    }

    pub fn effects(&self) -> impl Iterator<Item = &EffectDecl> {
        self.top_levels
            .iter()
            .filter_map(|top_level| match top_level {
                TopLevel::Effect(effect) => Some(&**effect),
                _ => None,
            })
    }
}
