use vulpi_intern::Symbol;
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
    r#type::Type,
    tree::Pattern,
    Lower, Parenthesis, Path, Upper,
};

#[derive(Show, Clone)]
pub struct Binder {
    pub left_paren: Token,
    pub pattern: Box<Pattern>,
    pub colon: Token,
    pub typ: Box<Type>,
    pub right_paren: Token,
}

#[derive(Show, Clone)]
pub struct LetCase {
    pub pipe: Token,
    pub arm: PatternArm,
}

#[derive(Show, Clone)]
pub enum LetMode {
    Body(Token, Box<Expr>),
    Cases(Vec<LetCase>),
}

#[derive(Show, Clone)]
pub struct LetSignature {
    pub visibility: Visibility,
    pub let_: Token,
    pub name: Lower,
    pub binders: Vec<Binder>,
    pub ret: Option<(Token, Box<Type>)>,
}

#[derive(Show, Clone)]
pub struct TraitDecl {
    pub visibility: Visibility,
    pub trait_: Token,
    pub name: Upper,
    pub binders: Vec<TypeBinder>,
    pub where_: Token,
    pub body: Vec<LetSignature>,
}

#[derive(Show, Clone)]
pub struct TraitImpl {
    pub impl_: Token,
    pub name: Upper,
    pub types: Vec<Box<Type>>,
    pub where_: Token,
    pub body: Vec<LetDecl>,
}

#[derive(Show, Clone)]
pub struct LetDecl {
    pub signature: LetSignature,
    pub body: LetMode,
}

#[derive(Show, Clone)]
pub struct Constructor {
    pub pipe: Token,
    pub name: Upper,
    pub args: Vec<Box<Type>>,
    pub typ: Option<(Token, Box<Type>)>,
}

#[derive(Show, Clone)]
pub struct SumDecl {
    pub constructors: Vec<Constructor>,
}

#[derive(Show, Clone)]
pub struct Field {
    pub visibility: Visibility,
    pub name: Lower,
    pub colon: Token,
    pub ty: Box<Type>,
}

#[derive(Show, Clone)]
pub struct RecordDecl {
    pub left_brace: Token,
    pub fields: Vec<(Field, Option<Token>)>,
    pub right_brace: Token,
}

#[derive(Show, Clone)]
pub struct ExplicitTypeBinder {
    pub name: Lower,
    pub colon: Token,
    pub kind: Box<Kind>,
}

#[derive(Show, Clone)]
pub enum TypeBinder {
    Implicit(Lower),
    Explicit(Parenthesis<ExplicitTypeBinder>),
}

#[derive(Show, Clone)]
pub enum TypeDef {
    Sum(SumDecl),
    Record(RecordDecl),
    Synonym(Box<Type>),
}

#[derive(Show, Clone)]
pub struct TypeDecl {
    pub visibility: Visibility,
    pub type_: Token,
    pub name: Upper,
    pub binders: Vec<TypeBinder>,
    pub def: Option<(Token, TypeDef)>,
}

#[derive(Show, Clone)]
pub struct UseAlias {
    pub as_: Token,
    pub alias: Upper,
}

#[derive(Show, Clone)]
pub struct UseDecl {
    pub visibility: Visibility,
    pub use_: Token,
    pub path: Path<Upper>,
    pub alias: Option<UseAlias>,
}

#[derive(Show, Clone)]
pub struct ModuleInline {
    pub name: Upper,
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

#[derive(Show, Clone)]
pub struct ModuleDecl {
    pub visibility: Visibility,
    pub mod_: Token,
    pub name: Upper,
    pub part: Option<ModuleInline>,
}

impl ModuleDecl {
    fn declares(&self, mut path: Vec<Symbol>) -> Vec<Vec<Symbol>> {
        if let Some(module) = &self.part {
            let mut paths = Vec::new();

            for module in module.modules() {
                path.push(module.name.symbol());
                paths.extend(module.declares(path.clone()));
            }
            
            paths
        } else {
            path.push(self.name.symbol());
            vec![path]
        }
    }
}

#[derive(Show, Clone)]
pub struct ExtDecl {
    pub visibility: Visibility,
    pub external: Token,
    pub name: Lower,
    pub colon: Token,
    pub typ: Box<Type>,
    pub equal: Token,
    pub str: Token,
}

#[derive(Show, Clone)]
pub enum TopLevel {
    Let(Box<LetDecl>),
    Type(Box<TypeDecl>),
    Use(Box<UseDecl>),
    Impl(Box<TraitImpl>),
    Trait(Box<TraitDecl>),
    Module(Box<ModuleDecl>),
    Error(Vec<Token>),
    External(Box<ExtDecl>),
}

#[derive(Show, Clone)]
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

    pub fn declares(&self) -> Vec<Vec<Symbol>> {
        let mut dependencies = Vec::new();

        for module in self.modules() {
            dependencies.extend(module.declares(Vec::new()));
        }

        dependencies
    }
}
