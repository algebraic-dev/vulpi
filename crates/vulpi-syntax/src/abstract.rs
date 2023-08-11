use std::collections::HashSet;

use vulpi_intern::Symbol;
use vulpi_location::Spanned;
use vulpi_macros::Show;

use vulpi_show::{Show, TreeDisplay};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    Effect,
    Constraint,
    Arrow(Kind, Kind),
    Error,
}

pub type Kind = Box<Spanned<KindType>>;

// Types

#[derive(Show)]
pub struct Effects {
    pub effects: Vec<Type>,
    pub rest: Option<Type>,
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

                if let Some(effs) = &pi.effects {
                    for eff in &effs.effects {
                        set.extend(eff.data.free_variables());
                    }
                }

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

    pub fn free_effects(&self) -> HashSet<Symbol> {
        match self {
            TypeKind::Arrow(pi) => {
                let mut set = pi.left.data.free_effects();
                set.extend(pi.right.data.free_effects());

                if let Some(effs) = &pi.effects {
                    for eff in &effs.effects {
                        set.extend(eff.data.free_effects());
                    }
                    if let Some(rest) = &effs.rest {
                        set.extend(rest.data.free_variables());
                    }
                }

                set
            }
            TypeKind::Tuple(t) => {
                let mut set = HashSet::new();

                for ty in t {
                    set.extend(ty.data.free_effects());
                }

                set
            }
            TypeKind::Application(app) => {
                let mut set = app.func.data.free_effects();

                for arg in &app.args {
                    set.extend(arg.data.free_effects());
                }

                set
            }
            TypeKind::Forall(f) => {
                let mut set = HashSet::new();

                set.extend(f.body.data.free_effects());

                for binder in &f.params {
                    match binder {
                        TypeBinder::Implicit(p) => set.remove(p),
                        TypeBinder::Explicit(p, _) => set.remove(p),
                    };
                }

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
    Ascription(PatAscription),
    Or(PatOr),
    Application(PatApplication),
    Effect(PatEffect),

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
    pub name: Qualified,
    pub binders: Vec<Binder>,
    pub ret: Option<(Option<Effects>, Type)>,
    pub body: LetMode,
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
    pub fields: Vec<(Qualified, Type)>,
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
    pub namespace: Symbol,
    pub visibility: Visibility,
    pub name: Qualified,
    pub binders: Vec<TypeBinder>,
    pub def: TypeDef,
}

#[derive(Show)]
pub struct ModuleDecl {
    pub namespace: Symbol,
    pub visibility: Visibility,
    pub name: Symbol,
    pub decls: Option<Vec<TopLevelDecl>>,
}

impl ModuleDecl {
    pub fn types(&self) -> Option<impl Iterator<Item = &TypeDecl>> {
        self.decls.as_ref().map(|decls| {
            decls.iter().filter_map(|decl| match *decl {
                TopLevelDecl::Type(ref decl) => Some(&**decl),
                _ => None,
            })
        })
    }

    pub fn effects(&self) -> Option<impl Iterator<Item = &EffectDecl>> {
        self.decls.as_ref().map(|decls| {
            decls.iter().filter_map(|decl| match *decl {
                TopLevelDecl::Effect(ref decl) => Some(&**decl),
                _ => None,
            })
        })
    }

    pub fn externals(&self) -> Option<impl Iterator<Item = &ExternalDecl>> {
        self.decls.as_ref().map(|decls| {
            decls.iter().filter_map(|decl| match *decl {
                TopLevelDecl::External(ref decl) => Some(&**decl),
                _ => None,
            })
        })
    }

    pub fn lets(&self) -> Option<impl Iterator<Item = &LetDecl>> {
        self.decls.as_ref().map(|decls| {
            decls.iter().filter_map(|decl| match *decl {
                TopLevelDecl::Let(ref decl) => Some(&**decl),
                _ => None,
            })
        })
    }

    pub fn modules(&self) -> Option<impl Iterator<Item = &ModuleDecl>> {
        self.decls.as_ref().map(|decls| {
            decls.iter().filter_map(|decl| match *decl {
                TopLevelDecl::Module(ref decl) => Some(&**decl),
                _ => None,
            })
        })
    }
}

#[derive(Show)]
pub struct EffectField {
    pub visibility: Visibility,
    pub name: Qualified,
    pub args: Vec<Type>,
    pub ty: Type,
}

#[derive(Show)]
pub struct EffectDecl {
    pub namespace: Symbol,
    pub visibility: Visibility,
    pub name: Qualified,
    pub binders: Vec<TypeBinder>,
    pub effects: Vec<EffectField>,
}

#[derive(Show)]
pub struct ExternalDecl {
    pub name: Symbol,
    pub namespace: Symbol,
    pub visibility: Visibility,
    pub ty: Type,
    pub ret: Symbol,
}

#[derive(Show)]
pub enum TopLevelDecl {
    Let(Box<LetDecl>),
    Type(Box<TypeDecl>),
    Module(Box<ModuleDecl>),
    Effect(Box<EffectDecl>),
    External(Box<ExternalDecl>),
}

#[derive(Show)]
pub struct Module {
    pub decls: Vec<TopLevelDecl>,
}

impl Module {
    pub fn lets(&self) -> impl Iterator<Item = &LetDecl> {
        self.decls.iter().filter_map(|decl| match *decl {
            TopLevelDecl::Let(ref decl) => Some(&**decl),
            _ => None,
        })
    }

    pub fn externals(&self) -> impl Iterator<Item = &ExternalDecl> {
        self.decls.iter().filter_map(|decl| match *decl {
            TopLevelDecl::External(ref decl) => Some(&**decl),
            _ => None,
        })
    }

    pub fn modules(&self) -> impl Iterator<Item = &ModuleDecl> {
        self.decls.iter().filter_map(|decl| match *decl {
            TopLevelDecl::Module(ref decl) => Some(&**decl),
            _ => None,
        })
    }

    pub fn effects(&self) -> impl Iterator<Item = &EffectDecl> {
        self.decls.iter().filter_map(|decl| match *decl {
            TopLevelDecl::Effect(ref decl) => Some(&**decl),
            _ => None,
        })
    }

    pub fn types(&self) -> impl Iterator<Item = &TypeDecl> {
        self.decls.iter().filter_map(|decl| match *decl {
            TopLevelDecl::Type(ref decl) => Some(&**decl),
            _ => None,
        })
    }
}
