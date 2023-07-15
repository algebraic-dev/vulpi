use std::ops::Range;

pub use visitor::{Visitor, Walkable};
use vulpi_location::Byte;
use vulpi_location::Spanned;
use vulpi_macros::node;
use vulpi_macros::Tree;
use vulpi_show::{Show, TreeDisplay};
use vulpi_storage::interner::Symbol;

use vulpi_macros::node_of;

pub mod visitor;

#[derive(Debug, Clone)]
#[node_of]
pub struct Ident(#[not] pub Symbol, #[not] pub Range<Byte>);

impl Ident {
    pub fn generate(symbol: Symbol) -> Self {
        Self(symbol, Byte(0)..Byte(0))
    }
}

impl Show for Ident {
    fn show(&self) -> vulpi_show::TreeDisplay {
        TreeDisplay::label(&self.0.get())
    }
}

#[derive(Debug, Clone, Tree)]
#[node_of]
pub struct Qualified {
    #[not]
    pub segments: Vec<Ident>,
    #[not]
    pub last: Ident,
    #[not]
    pub range: Range<Byte>,
}

// Node

pub trait Node: Show {
    fn accept(&mut self, visitor: &mut dyn Visitor);
}

impl<T: Node> Node for Spanned<T> {
    fn accept(&mut self, visitor: &mut dyn Visitor) {
        self.data.accept(visitor);
    }
}

impl Show for Box<dyn Node> {
    fn show(&self) -> TreeDisplay {
        (**self).show()
    }
}

impl Node for Box<dyn Node> {
    fn accept(&mut self, visitor: &mut dyn Visitor) {
        (**self).accept(visitor);
    }
}

impl<T: Node> Node for Vec<T> {
    fn accept(&mut self, visitor: &mut dyn Visitor) {
        for item in self {
            item.accept(visitor);
        }
    }
}

impl<T: Node> Node for Option<T> {
    fn accept(&mut self, visitor: &mut dyn Visitor) {
        if let Some(item) = self {
            item.accept(visitor);
        }
    }
}

impl<T: Walkable> Walkable for Spanned<T> {
    fn walk(&mut self, visitor: &mut dyn Visitor) {
        self.data.walk(visitor);
    }
}

impl<T: ?Sized + Walkable> Walkable for Box<T> {
    fn walk(&mut self, visitor: &mut dyn Visitor) {
        (**self).walk(visitor);
    }
}

// Types

#[node]
pub trait TypeImpl: Show + Node + Walkable {}

pub type Type = Spanned<Box<dyn TypeImpl>>;

#[derive(Default, Tree)]
pub struct Effects {
    pub effects: Vec<Type>,
}

#[derive(Tree)]
#[node_of(TypeImpl)]
pub struct UpperType(pub Qualified);

#[derive(Tree)]
#[node_of(TypeImpl)]
pub struct LowerType(pub Ident);

#[derive(Tree)]
#[node_of(TypeImpl)]

pub struct TypeArrow {
    pub left: Type,
    #[not]
    pub effects: Effects,
    pub right: Type,
}

#[derive(Tree)]
#[node_of(TypeImpl)]
pub struct TypeApplication {
    pub left: Type,
    pub right: Vec<Type>,
}

#[node_of(TypeImpl)]
#[derive(Tree)]
pub struct TypeForall {
    #[not]
    pub params: Vec<Ident>,
    pub body: Type,
}

#[node_of(TypeImpl)]
#[derive(Tree)]
pub struct UnitType;

// Literals

#[node]
pub trait LiteralImpl: Show + Node {}

type Literal = Spanned<Box<dyn LiteralImpl>>;

#[node_of(LiteralImpl)]
#[derive(Tree)]
pub struct StrLiteral(pub Ident);

#[node_of(LiteralImpl)]
#[derive(Tree)]
pub struct IntLiteral(pub Ident);

#[node_of(LiteralImpl)]
#[derive(Tree)]
pub struct FloatLiteral(pub Ident);

#[node_of(LiteralImpl)]
#[derive(Tree)]
pub struct CharLiteral(pub Ident);

#[node_of(LiteralImpl)]
#[derive(Tree)]
pub struct UnitLiteral;

// Patterns

#[node]
pub trait PatternImpl: Show + Node {}

pub type Pattern = Spanned<Box<dyn PatternImpl>>;

#[node_of(PatternImpl)]
#[derive(Tree)]
pub struct PatWildcard;

#[node_of(PatternImpl)]
#[derive(Tree)]
pub struct PatUpper(pub Qualified);

#[node_of(PatternImpl)]
#[derive(Tree)]
pub struct PatLower(pub Ident);

#[node_of(PatternImpl)]
#[derive(Tree)]
pub struct PatLiteral(pub Literal);

#[node_of(PatternImpl)]
#[derive(Tree)]
pub struct PatAnnotation {
    pub pat: Pattern,
    pub ty: Type,
}

#[node_of(PatternImpl)]
#[derive(Tree)]
pub struct PatOr {
    pub left: Pattern,
    pub right: Pattern,
}

#[node_of(PatternImpl)]
#[derive(Tree)]
pub struct PatApplication {
    #[not]
    pub func: Qualified,
    pub args: Vec<Pattern>,
}

// Block

#[node]
pub trait StatementImpl: Show + Node {}

pub type Statement = Spanned<Box<dyn StatementImpl>>;

#[node_of(StatementImpl)]
#[derive(Tree)]
pub struct StmtExpr(pub Expr);

#[node_of(StatementImpl)]
#[derive(Tree)]
pub struct StmtLet {
    pub pat: Pattern,
    pub expr: Expr,
}

#[node_of(StatementImpl)]
#[derive(Tree)]
pub struct StmtError;

#[derive(Tree)]
#[node_of]
pub struct Block {
    pub statements: Vec<Statement>,
}

// Expressions

#[node]
pub trait ExprImpl: Show + Node {}

pub type Expr = Spanned<Box<dyn ExprImpl>>;

#[derive(Tree)]
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

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct ExprLiteral(pub Literal);

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct LambdaExpr {
    pub pattern: Vec<Pattern>,
    pub body: Expr,
}

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct ApplicationExpr {
    pub func: Expr,
    pub args: Vec<Expr>,
}

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct AcessorExpr {
    pub expr: Expr,

    #[not]
    pub field: Ident,
}

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct BinaryExpr {
    pub left: Expr,
    #[not]
    pub op: Operator,
    pub right: Expr,
}

#[derive(Tree)]
#[node_of]
pub struct Arm {
    pub pattern: Pattern,
    pub then: Expr,
}

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct WhenExpr {
    pub scrutinee: Expr,
    pub arms: Vec<Arm>,
}

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct AnnotationExpr {
    pub expr: Expr,
    pub ty: Type,
}

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct LetExpr {
    pub pat: Pattern,
    pub value: Expr,
    pub body: Expr,
}

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct BlockExpr(pub Block);

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct ExprIdent(pub Qualified);

// Top level

#[derive(Tree)]
#[node_of]
pub struct Binder {
    pub pattern: Pattern,
    pub ty: Type,
}

#[derive(Tree)]
#[node_of]
pub struct LetCase {
    #[not]
    pub name_range: Range<Byte>,
    pub patterns: Vec<Binder>,
    pub body: Box<Expr>,
}

#[derive(Tree)]
#[node_of]
pub struct LetDecl {
    pub name: Ident,
    pub cases: Vec<LetCase>,
}

#[derive(Tree)]
#[node_of]
pub struct Variant {
    pub name: Ident,
    pub args: Vec<Type>,
}

#[derive(Tree)]
#[node_of]
pub struct EnumDecl {
    pub variants: Vec<Variant>,
}

#[derive(Tree)]
#[node_of]
pub struct Field {
    pub name: Ident,
    pub ty: Box<Type>,
}

#[derive(Tree)]
#[node_of]
pub struct RecordDecl {
    pub fields: Vec<Field>,
}

#[derive(Tree)]
pub enum TypeDef {
    Enum(EnumDecl),
    Record(RecordDecl),
    Synonym(Type),
}

impl Node for TypeDef {
    fn accept(&mut self, visitor: &mut dyn Visitor) {
        visitor.visit_type_def(self);
    }
}

impl Walkable for TypeDef {
    fn walk(&mut self, visitor: &mut dyn Visitor) {
        match self {
            TypeDef::Enum(e) => e.walk(visitor),
            TypeDef::Record(r) => r.walk(visitor),
            TypeDef::Synonym(t) => (t.data).walk(visitor),
        }
    }
}

#[derive(Tree)]
#[node_of]
pub struct TypeDecl {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub def: TypeDef,
}

#[derive(Tree)]
#[node_of]
pub struct UseDecl {
    pub path: Qualified,
    pub alias: Option<Ident>,
}

#[derive(Tree)]
#[node_of]
pub struct Program {
    pub uses: Vec<UseDecl>,
    pub types: Vec<TypeDecl>,
    pub lets: Vec<LetDecl>,
}
