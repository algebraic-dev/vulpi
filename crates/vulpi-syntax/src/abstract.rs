use std::collections::{HashSet, HashMap};

use vulpi_intern::Symbol;
use vulpi_location::{Span, Spanned};
use vulpi_macros::Show;

use vulpi_show::{Show, TreeDisplay};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Qualified {
    pub path: Symbol,
    pub name: Symbol,
}

impl Qualified {
    pub fn mangle(&self) -> String {
        format!("{}__{}", self.path.get(), self.name.get()).replace(".", "__").replace("?", "INT")
    }
    
    pub fn to_string(&self) -> String {
        format!("{}.{}", self.path.get(), self.name.get())
    }
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
    Constraint,
    Arrow(Kind, Kind),
    Error,
}

pub type Kind = Box<Spanned<KindType>>;

// Types

#[derive(Show)]
pub struct PiType {
    pub left: Type,
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
    Arrow(PiType),
    Tuple(Vec<Type>),
    Application(TypeApplication),
    Forall(TypeForall),
    TypeVariable(Symbol),
    Type(Qualified),
    Unit,

    Error,
}

pub type Type = Box<Spanned<TypeKind>>;

impl TypeKind {
    pub fn free_variables(&self) -> HashSet<Symbol> {
        match self {
            TypeKind::Arrow(pi) => {
                let mut set = pi.left.data.free_variables();
                set.extend(pi.right.data.free_variables());

                set
            }
            TypeKind::Tuple(t) => {
                let mut set = HashSet::new();

                for ty in t {
                    set.extend(ty.data.free_variables());
                }

                set
            }
            TypeKind::Application(app) => {
                let mut set = app.func.data.free_variables();

                for arg in &app.args {
                    set.extend(arg.data.free_variables());
                }

                set
            }
            TypeKind::Forall(f) => {
                let mut set = HashSet::new();

                set.extend(f.body.data.free_variables());

                for binder in &f.params {
                    match binder {
                        TypeBinder::Implicit(p) => set.remove(p),
                        TypeBinder::Explicit(p, _) => set.remove(p),
                    };
                }

                set
            }
            TypeKind::TypeVariable(v) => {
                let mut set = HashSet::new();
                set.insert(v.clone());
                set
            }
            _ => HashSet::new(),
        }
    }

}

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
pub struct LetSttm {
    pub pat: Pattern,
    pub expr: Expr,
}

#[derive(Show)]
pub enum SttmKind {
    Let(LetSttm),
    Expr(Expr),
    Error,
}

pub type Sttm = Spanned<SttmKind>;

#[derive(Show)]
pub struct Block {
    pub sttms: Vec<Sttm>,
}

// Patterns

#[derive(Show)]
pub struct PatAscription {
    pub pat: Pattern,
    pub typ: Type,
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
pub enum PatternKind {
    Wildcard,
    Variable(Symbol),
    Literal(Literal),
    Tuple(Vec<Pattern>),
    Ascription(PatAscription),
    Or(PatOr),
    Application(PatApplication),

    Error,
}

pub type Pattern = Box<Spanned<PatternKind>>;

#[derive(Show)]
pub struct LambdaExpr {
    pub param: Pattern,
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
    pub patterns: Vec<Pattern>,
    pub expr: Expr,
    pub guard: Option<Expr>,
}

#[derive(Show)]
pub struct WhenExpr {
    pub scrutinee: Vec<Expr>,
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
    pub fields: Vec<(Span, Symbol, Expr)>,
}

#[derive(Show)]
pub struct RecordUpdate {
    pub expr: Expr,
    pub fields: Vec<(Span, Symbol, Expr)>,
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
    Tuple(Tuple),

    Error,
}

impl ExprKind {
    pub fn accumulate(self) -> ExprKind {
        match self {
            ExprKind::Application(app1) => match app1.func.data {
                ExprKind::Application(app2) => {
                    let mut args = app2.args;
                    args.extend(app1.args);
    
                    ExprKind::Application(ApplicationExpr {
                        func: app2.func,
                        args,
                        app: AppKind::Normal,
                    })
                }
                _ => ExprKind::Application(app1),
            },
            _ => self,
        }
    }
}

pub type Expr = Box<Spanned<ExprKind>>;

#[derive(Show, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Super,
    Private,
}

impl From<crate::concrete::top_level::Visibility> for Visibility {
    fn from(visibility: crate::concrete::tree::Visibility) -> Self {
        use crate::concrete::top_level::Visibility::*;
        match visibility {
            Public(_) => Visibility::Public,
            Private => Visibility::Private,
        }
    }
}

#[derive(Show)]
pub struct Binder {
    pub pat: Pattern,
    pub ty: Type,
}

#[derive(Show)]
pub struct LetSignature {
    pub span: Span,
    pub visibility: Visibility,
    pub name: Qualified,
    pub binders: Vec<Binder>,
    pub ret: Option<Type>,
}

#[derive(Show)]
pub struct TraitDecl {
    pub name: Qualified,
    pub binders: Vec<TypeBinder>,
    pub body: Vec<LetSignature>,
}

#[derive(Show)]
pub struct TraitImpl {
    pub name: Qualified,
    pub binders: Vec<Type>,
    pub body: Vec<LetDecl>,
}

#[derive(Show)]
pub struct LetDecl {
    pub signature: LetSignature,
    pub body: Vec<PatternArm>,
    pub constant: Option<HashMap<Qualified, Span>>
}

#[derive(Show)]
pub struct Constructor {
    pub name: Qualified,
    pub args: Vec<Type>,
    pub typ: Option<Type>,
}

#[derive(Show)]
pub struct SumDecl {
    pub constructors: Vec<Constructor>,
}

#[derive(Show)]
pub struct RecordDecl {
    pub fields: Vec<(Qualified, Type, Visibility)>,
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
    pub name: Qualified,
    pub namespace: Symbol,
    pub binders: Vec<TypeBinder>,
    pub def: TypeDef,
}

#[derive(Show)]
pub struct ModuleDecl {
    pub visibility: Visibility,
    pub name: Symbol,
    pub decls: Option<Program>,
}


#[derive(Show)]
pub struct ExtDecl {
    pub name: Qualified,
    pub visibility: Visibility,
    pub namespace: Symbol,
    pub ty: Type,
    pub ret: Symbol,
}

pub enum TopLevel {
    Let(LetDecl),
    Type(TypeDecl),
    Module(ModuleDecl),
    External(ExtDecl),
    Trait(TraitDecl),
    Impl(Option<TraitImpl>),
    Use
}

#[derive(Show, Default)]
pub struct Program {
    pub lets: Vec<LetDecl>,
    pub types: Vec<TypeDecl>,
    pub modules: Vec<ModuleDecl>,
    pub traits: Vec<TraitDecl>,
    pub impls: Vec<TraitImpl>,
    pub externals: Vec<ExtDecl>,
}
