use vulpi_intern::Symbol;
use vulpi_location::Spanned;
use vulpi_macros::Show;

use vulpi_show::{Show, TreeDisplay};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Qualified {
    pub path: usize,
    pub name: Symbol,
}

impl Show for Qualified {
    fn show(&self) -> TreeDisplay {
        TreeDisplay::label("Qualified")
            .with(TreeDisplay::label(&self.path.to_string()))
            .with(TreeDisplay::label(&self.name.get()))
    }
}

#[derive(Show)]
pub enum KindType {
    Star,
    Arrow(Kind, Kind),
}

pub type Kind = Box<Spanned<KindType>>;

// Types

#[derive(Show)]
pub struct Effects {
    pub effects: Vec<Type>,
}

#[derive(Show)]
pub struct PiType {
    pub left: Type,
    pub effects: Option<Effects>,
    pub right: Type,
}

#[derive(Show)]
pub struct TypeApplication {
    pub func: Type,
    pub args: Vec<Type>,
}

#[derive(Show)]
pub enum TypeBinder {
    Implicit(Symbol),
    Explicit(Symbol, Kind),
}

#[derive(Show)]
pub struct TypeForall {
    pub params: Vec<TypeBinder>,
    pub body: Type,
}

#[derive(Show)]
pub enum TypeKind {
    Pi(PiType),
    Tuple(Vec<Type>),
    Application(TypeApplication),
    Forall(TypeForall),
    TypeVariable(Symbol),
    Type(Qualified),
    Unit,

    Error,
}

pub type Type = Box<Spanned<TypeKind>>;

// Literal

#[derive(Show)]
pub enum LiteralKind {
    String(Symbol),
    Integer(Symbol),
    Float(Symbol),
    Char(Symbol),
    Unit,
}

pub type Literal = Box<Spanned<LiteralKind>>;

// Statements

#[derive(Show)]
pub struct LetStatement {
    pub pattern: Pattern,
    pub expr: Expr,
}

#[derive(Show)]
pub enum StatementKind {
    Let(LetStatement),
    Expr(Expr),
    Error,
}

pub type Statement = Spanned<StatementKind>;

#[derive(Show)]
pub struct Block {
    pub statements: Vec<Statement>,
}

// Patterns

#[derive(Show)]
pub struct PatAscription {
    pub left: Pattern,
    pub right: Type,
}

#[derive(Show)]
pub struct PatOr {
    pub left: Pattern,
    pub right: Pattern,
}

#[derive(Show)]
pub struct PatApplication {
    pub func: Qualified,
    pub args: Vec<Pattern>,
}

#[derive(Show)]
pub struct PatEffect {
    pub func: Qualified,
    pub args: Vec<Pattern>,
    pub cont: Option<Symbol>,
}

#[derive(Show)]
pub enum PatternKind {
    Wildcard,
    Variable(Symbol),
    Literal(Literal),
    Annotation(PatAscription),
    Or(PatOr),
    Application(PatApplication),
    Effect(PatEffect),

    Error,
}

pub type Pattern = Box<Spanned<PatternKind>>;

#[derive(Show)]
pub struct LambdaExpr {
    pub params: Vec<Pattern>,
    pub body: Expr,
}

#[derive(Show)]
pub enum AppKind {
    Infix,
    Normal,
}

#[derive(Show)]
pub struct ApplicationExpr {
    pub app: AppKind,
    pub func: Expr,
    pub args: Vec<Expr>,
}

#[derive(Show)]
pub struct ProjectionExpr {
    pub expr: Expr,
    pub field: Symbol,
}

#[derive(Show)]
pub struct PatternArm {
    pub pattern: Vec<Pattern>,
    pub expr: Expr,
    pub guard: Option<Expr>,
}

#[derive(Show)]
pub struct WhenExpr {
    pub scrutinee: Expr,
    pub arms: Vec<PatternArm>,
}

#[derive(Show)]
pub struct AnnotationExpr {
    pub expr: Expr,
    pub ty: Type,
}

#[derive(Show)]
pub struct LetExpr {
    pub pattern: Pattern,
    pub body: Expr,
    pub value: Expr,
}

#[derive(Show)]
pub struct RecordInstance {
    pub name: Qualified,
    pub fields: Vec<(Symbol, Expr)>,
}

#[derive(Show)]
pub struct RecordUpdate {
    pub expr: Expr,
    pub fields: Vec<(Symbol, Expr)>,
}

#[derive(Show)]
pub struct HandlerExpr {
    pub expr: Expr,
    pub with: Expr,
}

#[derive(Show)]
pub struct CasesExpr {
    pub arms: Vec<PatternArm>,
}

#[derive(Show)]
pub struct Tuple {
    pub exprs: Vec<Expr>,
}

#[derive(Show)]
pub enum ExprKind {
    Lambda(LambdaExpr),
    Application(ApplicationExpr),

    Variable(Symbol),
    Constructor(Qualified),
    Function(Qualified),
    Effect(Qualified),

    Projection(ProjectionExpr),
    Let(LetExpr),
    When(WhenExpr),
    Do(Block),
    Literal(Literal),

    Annotation(AnnotationExpr),
    RecordInstance(RecordInstance),
    RecordUpdate(RecordUpdate),
    Handler(HandlerExpr),
    Cases(CasesExpr),
    Tuple(Tuple),
    Unit,

    Error,
}

pub type Expr = Box<Spanned<ExprKind>>;

#[derive(Show)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Show)]
pub struct LetCase {
    pub pattern: PatternArm,
}

#[derive(Show)]
pub struct LetMode {
    pub cases: Vec<LetCase>,
}

#[derive(Show)]
pub struct Binder {
    pub pattern: Pattern,
    pub ty: Type,
}

#[derive(Show)]
pub struct LetDecl {
    pub visibility: Visibility,
    pub name: Symbol,
    pub binders: Vec<Binder>,
    pub ret: Option<(Option<Effects>, Type)>,
    pub body: LetMode,
}

#[derive(Show)]
pub struct Constructor {
    pub name: Symbol,
    pub args: Vec<Type>,
}

#[derive(Show)]
pub struct SumDecl {
    pub constructors: Vec<Constructor>,
}

#[derive(Show)]
pub struct RecordDecl {
    pub fields: Vec<(Symbol, Type)>,
}

#[derive(Show)]
pub enum TypeDef {
    Sum(SumDecl),
    Record(RecordDecl),
    Synonym(Type),
    Abstract,
}

#[derive(Show)]
pub struct TypeDecl {
    pub id: usize,
    pub visibility: Visibility,
    pub name: Symbol,
    pub binders: Vec<TypeBinder>,
    pub def: TypeDef,
}

#[derive(Show)]
pub struct ModuleDecl {
    pub id: usize,
    pub visibility: Visibility,
    pub name: Symbol,
    pub decls: Option<Vec<TopLevelDecl>>,
}

#[derive(Show)]
pub struct EffectField {
    pub visibility: Visibility,
    pub name: Symbol,
    pub args: Vec<Type>,
    pub ty: Type,
}

#[derive(Show)]
pub struct EffectDecl {
    pub id: usize,
    pub visibility: Visibility,
    pub name: Symbol,
    pub binders: Vec<TypeBinder>,
    pub fields: Vec<EffectField>,
}

#[derive(Show)]
pub enum TopLevelDecl {
    Let(Box<LetDecl>),
    Type(Box<TypeDecl>),
    Module(Box<ModuleDecl>),
    Effect(Box<EffectDecl>),
}

#[derive(Show)]
pub struct Module {
    pub decls: Vec<TopLevelDecl>,
}
