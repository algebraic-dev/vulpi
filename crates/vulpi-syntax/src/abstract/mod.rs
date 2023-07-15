use std::ops::Range;

pub use visitor::Visitor;
use vulpi_location::Byte;
use vulpi_location::Spanned;
use vulpi_macros::Tree;
use vulpi_show::{Show, TreeDisplay};
use vulpi_storage::interner::Symbol;

use vulpi_macros::node_of;

mod visitor;

#[derive(Debug, Clone)]
pub struct Ident(pub Symbol, pub Range<Byte>);

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
pub struct Qualified {
    pub segments: Vec<Ident>,
    pub last: Ident,
    pub range: Range<Byte>,
}

// Types

pub trait TypeImpl: Show {
    fn accept(&mut self, visitor: &mut dyn Visitor);
}

impl Show for Box<dyn TypeImpl> {
    fn show(&self) -> TreeDisplay {
        (**self).show()
    }
}

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

#[node_of(TypeImpl)]
#[derive(Tree)]
pub struct TypeArrow {
    pub left: Type,
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
    pub params: Vec<Ident>,
    pub body: Type,
}

#[node_of(TypeImpl)]
#[derive(Tree)]
pub struct UnitType;

// Literals

pub trait LiteralImpl: Show {
    fn accept(&mut self, visitor: &mut dyn Visitor);
}

impl Show for Box<dyn LiteralImpl> {
    fn show(&self) -> TreeDisplay {
        (**self).show()
    }
}

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

pub trait PatternImpl: Show {
    fn accept(&mut self, visitor: &mut dyn Visitor);
}

impl Show for Box<dyn PatternImpl> {
    fn show(&self) -> TreeDisplay {
        (**self).show()
    }
}

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
    pub pat: Box<Pattern>,
    pub ty: Type,
}

#[node_of(PatternImpl)]
#[derive(Tree)]
pub struct PatOr {
    pub left: Box<Pattern>,
    pub right: Box<Pattern>,
}

#[node_of(PatternImpl)]
#[derive(Tree)]
pub struct PatApplication {
    pub func: Qualified,
    pub args: Vec<Pattern>,
}

// Block

pub trait StatementImpl: Show {
    fn accept(&mut self, visitor: &mut dyn Visitor);
}

impl Show for Box<dyn StatementImpl> {
    fn show(&self) -> TreeDisplay {
        (**self).show()
    }
}

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
pub struct Block {
    pub statements: Vec<Statement>,
}

// Expressions

pub trait ExprImpl: Show {
    fn accept(&mut self, visitor: &mut dyn Visitor);
}

impl Show for Box<dyn ExprImpl> {
    fn show(&self) -> TreeDisplay {
        (**self).show()
    }
}

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
    pub field: Ident,
}

#[node_of(ExprImpl)]
#[derive(Tree)]
pub struct BinaryExpr {
    pub left: Expr,
    pub op: Operator,
    pub right: Expr,
}

#[derive(Tree)]
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
pub struct LetCase {
    pub name_range: Range<Byte>,
    pub patterns: Vec<(Pattern, Type)>,
    pub body: Box<Expr>,
}

#[derive(Tree)]
pub struct LetDecl {
    pub name: Ident,
    pub cases: Vec<LetCase>,
}

#[derive(Tree)]
pub struct Variant {
    pub name: Ident,
    pub args: Vec<Type>,
}

#[derive(Tree)]
pub struct EnumDecl {
    pub variants: Vec<Variant>,
}

#[derive(Tree)]
pub struct Field {
    pub name: Ident,
    pub ty: Box<Type>,
}

#[derive(Tree)]
pub struct RecordDecl {
    pub fields: Vec<Field>,
}

#[derive(Tree)]
pub enum TypeDef {
    Enum(EnumDecl),
    Record(RecordDecl),
    Synonym(Type),
}

#[derive(Tree)]
pub struct TypeDecl {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub def: TypeDef,
}

#[derive(Tree)]
pub struct UseDecl {
    pub path: Qualified,
    pub alias: Option<Ident>,
}

#[derive(Tree)]
pub struct Program {
    pub uses: Vec<UseDecl>,
    pub types: Vec<TypeDecl>,
    pub lets: Vec<LetDecl>,
}
