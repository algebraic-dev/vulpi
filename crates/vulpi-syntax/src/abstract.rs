use vulpi_intern::Symbol;
use vulpi_location::Spanned;
use vulpi_macros::Show;

use vulpi_show::{Show, TreeDisplay};

pub struct Qualified {
    pub path: Symbol,
    pub name: Symbol,
}

impl Show for Qualified {
    fn show(&self) -> TreeDisplay {
        TreeDisplay::label("Qualified")
            .with(TreeDisplay::label(&self.path.get()))
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
    pub pattern: Box<Pattern>,
    pub expr: Box<Expr>,
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
    pub left: Box<Pattern>,
    pub right: Type,
}

#[derive(Show)]
pub struct PatOr {
    pub left: Box<Pattern>,
    pub right: Box<Pattern>,
}

#[derive(Show)]
pub struct PatApplication {
    pub func: Qualified,
    pub args: Vec<Box<Pattern>>,
}

#[derive(Show)]
pub struct PatEffectApp {
    pub func: Qualified,
    pub args: Vec<Box<Pattern>>,
}

#[derive(Show)]
pub enum PatternKind {
    Wildcard,
    Constructor(Qualified),
    Variable(Symbol),
    Literal(Literal),
    Annotation(PatAscription),
    Or(PatOr),
    Application(PatApplication),
    EffectApp(PatEffectApp),
}

pub type Pattern = Spanned<PatternKind>;

#[derive(Show)]
pub struct LambdaExpr {
    pub params: Vec<Pattern>,
    pub body: Box<Expr>,
}

#[derive(Show)]
pub enum AppKind {
    Infix,
    Normal,
}

#[derive(Show)]
pub struct ApplicationExpr {
    pub app: AppKind,
    pub func: Box<Expr>,
    pub args: Vec<Box<Expr>>,
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
pub struct CasesArm {
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

    Projection(ProjectionExpr),
    Let(LetExpr),
    When(WhenExpr),
    Do(Block),
    Literal(Literal),

    Annotation(AnnotationExpr),
    RecordInstance(RecordInstance),
    RecordUpdate(RecordUpdate),
    Handler(HandlerExpr),
    Cases(CasesArm),
    Tuple(Tuple),
    Unit,
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
    pub expr: Expr,
}

#[derive(Show)]
pub struct LetMode {
    pub cases: Vec<LetCase>,
    pub body: Expr,
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
    pub visibility: Visibility,
    pub name: Symbol,
    pub binders: Vec<TypeBinder>,
    pub def: TypeDef,
}

#[derive(Show)]
pub struct UseDecl {
    pub visibility: Visibility,
    pub path: Symbol,
    pub alias: Option<Symbol>,
}

#[derive(Show)]
pub struct ModuleDecl {
    pub visibility: Visibility,
    pub name: Symbol,
    pub decls: Vec<TopLevelDecl>,
}

#[derive(Show)]
pub struct EffectField {
    pub name: Symbol,
    pub args: Vec<Type>,
    pub ty: Type,
}

#[derive(Show)]
pub struct EffectDecl {
    pub visibility: Visibility,
    pub name: Symbol,
    pub binders: Vec<TypeBinder>,
    pub fields: Vec<EffectField>,
}

#[derive(Show)]
pub enum TopLevelDecl {
    Let(LetDecl),
    Type(TypeDecl),
    Use(UseDecl),
    Module(ModuleDecl),
    Effect(EffectDecl),
}

#[derive(Show)]
pub struct Module {
    pub decls: Vec<TopLevelDecl>,
}