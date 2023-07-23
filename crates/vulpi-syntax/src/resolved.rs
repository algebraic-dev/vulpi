use std::ops::Range;

use vulpi_location::{Byte, Spanned};
use vulpi_macros::Tree;
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;

pub type Ident = Spanned<Symbol>;

#[derive(Debug, Clone, Tree)]
pub struct Qualified {
    pub canonical: Id<id::Namespace>,
    pub last: Symbol,
    pub range: Range<Byte>,
}

impl PartialEq for Qualified {
    fn eq(&self, other: &Self) -> bool {
        self.canonical == other.canonical && self.last == other.last
    }
}

impl Eq for Qualified {}

impl std::hash::Hash for Qualified {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.canonical.hash(state);
        self.last.hash(state);
    }
}

impl Qualified {
    pub fn get_range(&self) -> Range<Byte> {
        self.range.clone()
    }
}

#[derive(Default, Tree, Debug)]
pub struct Effects {
    pub effects: Vec<Type>,
}

/// The arrow type `A -> B`
#[derive(Tree, Debug)]
pub struct TypeArrow {
    pub left: Box<Type>,
    pub effects: Effects,
    pub right: Box<Type>,
}

/// The application type `A B`
#[derive(Tree, Debug)]
pub struct TypeApplication {
    pub fun: Box<Type>,
    pub args: Vec<Type>,
}

/// The forall type `forall a b. A -> B`
#[derive(Tree, Debug)]
pub struct TypeForall {
    pub params: Vec<Ident>,
    pub body: Box<Type>,
}

/// The type kind `A`, `A -> B`, `forall a b. A -> B`
#[derive(Tree, Debug)]
pub enum TypeKind {
    Upper(Qualified),
    Lower(Ident),
    Arrow(TypeArrow),
    Application(TypeApplication),
    Forall(TypeForall),
    Unit,
    Error,
}

pub type Type = Spanned<TypeKind>;

// Literal

#[derive(Tree, Debug)]
pub enum LiteralKind {
    String(Ident, Qualified),
    Integer(Ident, Qualified),
    Char(Ident, Qualified),
    Float(Ident, Qualified),
    Unit,
    Error,
}

pub type Literal = Spanned<LiteralKind>;

// Pattern

#[derive(Tree, Debug)]
pub struct PatAnnotation {
    pub pat: Box<Pattern>,
    pub ty: Box<Type>,
}

#[derive(Tree, Debug)]
pub struct PatOr {
    pub left: Box<Pattern>,
    pub right: Box<Pattern>,
}

#[derive(Tree, Debug)]
pub struct PatApplication {
    pub func: Qualified,
    pub args: Vec<Pattern>,
}

#[derive(Tree, Debug)]
pub enum PatternKind {
    Wildcard,
    Lower(Ident),
    Literal(Literal),
    Annotation(PatAnnotation),
    Or(PatOr),
    Application(PatApplication),
    Error,
}

pub type Pattern = Spanned<PatternKind>;

// Expression

#[derive(Tree, Debug)]
pub struct LetStmt {
    pub name: Box<Pattern>,
    pub expr: Box<Expr>,
}

#[derive(Tree, Debug)]
pub enum StatementKind {
    Let(LetStmt),
    Expr(Expr),
    Error,
}

pub type Statement = Spanned<StatementKind>;

#[derive(Tree, Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Tree, Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Not,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Pipe,
}

#[derive(Tree, Debug)]
pub struct LambdaExpr {
    pub pattern: Pattern,
    pub body: Box<Expr>,
}

#[derive(Tree, Debug)]
pub struct ApplicationExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Tree, Debug)]
pub struct AcessorExpr {
    pub expr: Box<Expr>,
    pub field: Ident,
}

#[derive(Tree, Debug)]
pub struct WhenArm {
    pub pattern: Box<Pattern>,
    pub then: Box<Expr>,
}

#[derive(Tree, Debug)]
pub struct WhenExpr {
    pub scrutinee: Box<Expr>,
    pub arms: Vec<WhenArm>,
}

#[derive(Tree, Debug)]
pub struct AnnotationExpr {
    pub expr: Box<Expr>,
    pub ty: Box<Type>,
}

#[derive(Tree, Debug)]
pub struct LetExpr {
    pub name: Box<Pattern>,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Tree, Debug)]
pub struct RecordInstance {
    pub fields: Vec<(Ident, Expr)>,
    pub name: Qualified,
}

#[derive(Tree, Debug)]
pub struct RecordUpdate {
    pub fields: Vec<(Ident, Expr)>,
    pub expr: Box<Expr>,
}

#[derive(Tree, Debug)]
pub enum ExprKind {
    Variable(Ident),

    Function(Qualified),
    Constructor(Qualified),

    Lambda(LambdaExpr),
    Application(ApplicationExpr),
    Acessor(AcessorExpr),
    Let(LetExpr),
    When(WhenExpr),
    Annotation(AnnotationExpr),
    Block(Block),
    Literal(Literal),

    RecordInstance(RecordInstance),
    RecordUpdate(RecordUpdate),

    Error,
}

pub type Expr = Spanned<ExprKind>;

// Top level

#[derive(Tree, Debug)]
pub struct LetCase {
    pub patterns: Vec<Pattern>,
    pub body: Box<Expr>,
    pub range: Range<Byte>,
}

#[derive(Tree, Debug)]
pub struct LetDecl {
    pub name: Ident,
    pub params: Vec<(Pattern, Type)>,
    pub cases: Vec<LetCase>,
    pub ret: Option<Type>,
}

#[derive(Tree, Debug)]
pub struct Variant {
    pub name: Ident,
    pub args: Vec<Type>,
}

#[derive(Tree, Debug)]
pub struct EnumDecl {
    pub variants: Vec<Variant>,
}

#[derive(Tree, Debug)]
pub struct Field {
    pub name: Ident,
    pub ty: Box<Type>,
}

#[derive(Tree, Debug)]
pub struct RecordDecl {
    pub fields: Vec<Field>,
}

#[derive(Tree, Debug)]
pub enum TypeDef {
    Enum(EnumDecl),
    Record(RecordDecl),
    Synonym(Type),
}

#[derive(Tree, Debug)]
pub struct TypeDecl {
    pub id: Id<id::Namespace>,
    pub name: Ident,
    pub params: Vec<Ident>,
    pub def: TypeDef,
}

#[derive(Tree, Debug)]
pub struct Program {
    pub id: Id<id::Namespace>,
    pub types: Vec<TypeDecl>,
    pub lets: Vec<LetDecl>,
}
