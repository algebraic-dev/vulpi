use declare::Context;
use error::{ResolverError, ResolverErrorKind};
use namespace::{Item, TypeValue, Value};
use vulpi_intern::Symbol;
use vulpi_location::{Span, Spanned};

use vulpi_syntax::concrete::{tree::*, Lower, Path, Upper};
use vulpi_syntax::r#abstract as abs;
use vulpi_syntax::r#abstract::Qualified;

pub mod declare;
pub mod error;
pub mod module_tree;
pub mod namespace;

pub trait Resolve {
    type Output;

    fn resolve(self, ctx: &mut Context) -> Self::Output;
}

impl<T: Resolve> Resolve for Vec<T> {
    type Output = Vec<T::Output>;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        self.into_iter().map(|x| x.resolve(ctx)).collect()
    }
}

impl<T: Resolve> Resolve for Spanned<T> {
    type Output = Spanned<T::Output>;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        Spanned::new(self.data.resolve(ctx), self.span)
    }
}

impl<T: Resolve> Resolve for Box<T> {
    type Output = Box<T::Output>;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        Box::new((*self).resolve(ctx))
    }
}

impl<T: Resolve> Resolve for Option<T> {
    type Output = Option<T::Output>;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        self.map(|x| x.resolve(ctx))
    }
}

impl Resolve for KindType {
    type Output = abs::KindType;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        match self {
            KindType::Star(_) => abs::KindType::Star,
            KindType::Parenthesis(n) => n.data.resolve(ctx).data,
            KindType::Arrow(left, _, right) => {
                abs::KindType::Arrow(left.resolve(ctx), right.resolve(ctx))
            }
        }
    }
}

impl Resolve for Effects {
    type Output = abs::Effects;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::Effects {
            effects: self.effects.into_iter().map(|x| x.0.resolve(ctx)).collect(),
        }
    }
}

impl Resolve for TypeArrow {
    type Output = abs::PiType;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::PiType {
            left: self.left.resolve(ctx),
            effects: self.effects.resolve(ctx),
            right: self.right.resolve(ctx),
        }
    }
}

impl Resolve for TypeApplication {
    type Output = abs::TypeApplication;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::TypeApplication {
            func: self.func.resolve(ctx),
            args: self.args.resolve(ctx),
        }
    }
}

impl Resolve for TypeForall {
    type Output = abs::TypeForall;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::TypeForall {
            params: self.params.resolve(ctx),
            body: self.body.resolve(ctx),
        }
    }
}

impl Resolve for TypeKind {
    type Output = abs::TypeKind;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        match self {
            TypeKind::Type(n) => {
                let vec: Vec<_> = (&n).into();
                match ctx.find_type(n.span, &vec) {
                    Some(Item { item, .. }) => abs::TypeKind::Type(item.qualified().clone()),
                    None => abs::TypeKind::Error,
                }
            }
            TypeKind::TypeVariable(n) => abs::TypeKind::TypeVariable(n.symbol()),
            TypeKind::Parenthesis(n) => n.data.0.resolve(ctx).data,
            TypeKind::Arrow(n) => abs::TypeKind::Pi(n.resolve(ctx)),
            TypeKind::Application(n) => abs::TypeKind::Application(n.resolve(ctx)),
            TypeKind::Forall(n) => abs::TypeKind::Forall(n.resolve(ctx)),
            TypeKind::Unit(_) => abs::TypeKind::Unit,
            TypeKind::Tuple(n) => {
                abs::TypeKind::Tuple(n.data.into_iter().map(|x| x.0.resolve(ctx)).collect())
            }
        }
    }
}

impl Resolve for LiteralKind {
    type Output = abs::LiteralKind;

    fn resolve(self, _: &mut Context) -> Self::Output {
        match self {
            LiteralKind::String(n) => abs::LiteralKind::String(n.symbol()),
            LiteralKind::Integer(n) => abs::LiteralKind::Integer(n.symbol()),
            LiteralKind::Float(n) => abs::LiteralKind::Float(n.symbol()),
            LiteralKind::Char(n) => abs::LiteralKind::Char(n.symbol()),
            LiteralKind::Unit(_) => abs::LiteralKind::Unit,
        }
    }
}

impl Resolve for PatternKind {
    type Output = abs::PatternKind;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        match self {
            PatternKind::Variable(n) => abs::PatternKind::Variable(n.symbol()),
            PatternKind::Constructor(n) => PatternKind::Application(PatApplication {
                func: n,
                args: vec![],
            })
            .resolve(ctx),
            PatternKind::Wildcard(_) => abs::PatternKind::Wildcard,
            PatternKind::Literal(n) => abs::PatternKind::Literal(Box::new(n.resolve(ctx))),
            PatternKind::Annotation(n) => abs::PatternKind::Annotation(n.resolve(ctx)),
            PatternKind::Or(n) => abs::PatternKind::Or(n.resolve(ctx)),
            PatternKind::Application(n) => n.resolve(ctx),
            PatternKind::EffectApp(n) => n.resolve(ctx),
            PatternKind::Parenthesis(n) => n.data.resolve(ctx).data,
        }
    }
}

impl Resolve for PatAscription {
    type Output = abs::PatAscription;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::PatAscription {
            left: self.left.resolve(ctx),
            right: self.right.resolve(ctx),
        }
    }
}

impl Resolve for PatOr {
    type Output = abs::PatOr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::PatOr {
            left: self.left.resolve(ctx),
            right: self.right.resolve(ctx),
        }
    }
}

impl Resolve for PatApplication {
    type Output = abs::PatternKind;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        let func: Vec<_> = (&self.func).into();
        let func = match ctx.find_value(self.func.span.clone(), &func) {
            Some(Item {
                item: Value::Constructor(qual),
                ..
            }) => qual,
            Some(_) => {
                ctx.report(ResolverError {
                    span: self.func.span,
                    kind: ResolverErrorKind::ExpectedConstructor,
                });
                return abs::PatternKind::Error;
            }
            None => {
                return abs::PatternKind::Error;
            }
        };

        abs::PatternKind::Application(abs::PatApplication {
            func,
            args: self.args.resolve(ctx),
        })
    }
}

impl Resolve for PatEffectApp {
    type Output = abs::PatternKind;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        let func: Vec<_> = (&self.func).into();
        let func = match ctx.find_value(self.func.span.clone(), &func) {
            Some(Item {
                item: Value::Constructor(qual),
                ..
            }) => qual,
            Some(_) => {
                ctx.report(ResolverError {
                    span: self.func.span,
                    kind: ResolverErrorKind::ExpectedConstructor,
                });
                return abs::PatternKind::Error;
            }
            None => {
                return abs::PatternKind::Error;
            }
        };

        abs::PatternKind::Effect(abs::PatEffect {
            func,
            args: self.args.resolve(ctx),
        })
    }
}

impl Resolve for LambdaExpr {
    type Output = abs::LambdaExpr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::LambdaExpr {
            params: self.patterns.resolve(ctx),
            body: self.expr.resolve(ctx),
        }
    }
}

impl Resolve for ApplicationExpr {
    type Output = abs::ApplicationExpr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::ApplicationExpr {
            app: abs::AppKind::Normal,
            func: self.func.resolve(ctx),
            args: self.args.resolve(ctx),
        }
    }
}

impl Resolve for ProjectionExpr {
    type Output = abs::ProjectionExpr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::ProjectionExpr {
            expr: self.expr.resolve(ctx),
            field: self.field.symbol(),
        }
    }
}

impl Resolve for Operator {
    type Output = abs::Expr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        let (span, name) = match self {
            Operator::Add(token) => (token.value.span, "add"),
            Operator::Sub(token) => (token.value.span, "sub"),
            Operator::Mul(token) => (token.value.span, "mul"),
            Operator::Div(token) => (token.value.span, "div"),
            Operator::Rem(token) => (token.value.span, "rem"),
            Operator::And(token) => (token.value.span, "and"),
            Operator::Or(token) => (token.value.span, "or"),
            Operator::Xor(token) => (token.value.span, "xor"),
            Operator::Not(token) => (token.value.span, "not"),
            Operator::Eq(token) => (token.value.span, "eq"),
            Operator::Neq(token) => (token.value.span, "neq"),
            Operator::Lt(token) => (token.value.span, "lt"),
            Operator::Gt(token) => (token.value.span, "gt"),
            Operator::Le(token) => (token.value.span, "le"),
            Operator::Ge(token) => (token.value.span, "ge"),
            Operator::Shl(token) => (token.value.span, "shl"),
            Operator::Shr(token) => (token.value.span, "shr"),
            Operator::Pipe(token) => (token.value.span, "pipe"),
        };

        let x = &[Symbol::intern("Operator"), Symbol::intern(name)];

        let kind = match ctx.find_value(span.clone(), x) {
            Some(Item {
                item: Value::Function(qual),
                ..
            }) => abs::ExprKind::Function(qual),
            Some(_) => {
                ctx.report(ResolverError {
                    span: span.clone(),
                    kind: ResolverErrorKind::ExpectedConstructor,
                });
                abs::ExprKind::Error
            }
            None => abs::ExprKind::Error,
        };

        Box::new(Spanned::new(kind, span))
    }
}

impl Resolve for BinaryExpr {
    type Output = abs::ExprKind;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::ExprKind::Application(abs::ApplicationExpr {
            func: self.op.resolve(ctx),
            args: vec![self.left.resolve(ctx), self.right.resolve(ctx)],
            app: abs::AppKind::Infix,
        })
    }
}

impl Resolve for IfExpr {
    type Output = abs::ExprKind;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        let fn_cons = |name| {
            find_constructor_raw(
                self.if_.value.span.clone(),
                &[Symbol::intern("Bool"), Symbol::intern(name)],
                ctx,
                |func| abs::PatternKind::Application(abs::PatApplication { func, args: vec![] }),
                abs::PatternKind::Error,
            )
        };

        let true_cons = fn_cons("true");
        let false_cons = fn_cons("false");

        abs::ExprKind::When(abs::WhenExpr {
            scrutinee: self.cond.resolve(ctx),
            arms: vec![
                abs::PatternArm {
                    pattern: vec![Box::new(Spanned::new(
                        true_cons,
                        self.if_.value.span.clone(),
                    ))],
                    expr: self.then_expr.resolve(ctx),
                    guard: None,
                },
                abs::PatternArm {
                    pattern: vec![Box::new(Spanned::new(
                        false_cons,
                        self.if_.value.span.clone(),
                    ))],
                    expr: self.else_expr.resolve(ctx),
                    guard: None,
                },
            ],
        })
    }
}

impl Resolve for PatternArm {
    type Output = abs::PatternArm;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::PatternArm {
            pattern: self
                .patterns
                .into_iter()
                .map(|x| x.0.resolve(ctx))
                .collect(),
            expr: self.expr.resolve(ctx),
            guard: self.guard.map(|x| x.1).resolve(ctx),
        }
    }
}

impl Resolve for WhenExpr {
    type Output = abs::WhenExpr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::WhenExpr {
            scrutinee: self.scrutinee.resolve(ctx),
            arms: self.arms.resolve(ctx),
        }
    }
}

impl Resolve for AnnotationExpr {
    type Output = abs::AnnotationExpr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::AnnotationExpr {
            expr: self.expr.resolve(ctx),
            ty: self.ty.resolve(ctx),
        }
    }
}

impl Resolve for LetExpr {
    type Output = abs::LetExpr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::LetExpr {
            pattern: self.pattern.resolve(ctx),
            body: self.body.resolve(ctx),
            value: self.value.resolve(ctx),
        }
    }
}

impl Resolve for RecordField {
    type Output = (Symbol, abs::Expr);

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        (self.name.symbol(), self.expr.resolve(ctx))
    }
}

impl Resolve for RecordInstance {
    type Output = abs::ExprKind;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        let vec: Vec<_> = (&self.name).into();

        let name = match ctx.find_type(self.name.span.clone(), &vec) {
            Some(Item {
                item: TypeValue::Record(n),
                ..
            }) => n,
            Some(_) => {
                ctx.report(ResolverError {
                    span: self.name.span,
                    kind: ResolverErrorKind::ExpectedRecordType,
                });
                return abs::ExprKind::Error;
            }
            None => return abs::ExprKind::Error,
        };

        abs::ExprKind::RecordInstance(abs::RecordInstance {
            name,
            fields: self.fields.into_iter().map(|x| x.0.resolve(ctx)).collect(),
        })
    }
}

impl Resolve for RecordUpdate {
    type Output = abs::RecordUpdate;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::RecordUpdate {
            expr: self.expr.resolve(ctx),
            fields: self.fields.into_iter().map(|x| x.0.resolve(ctx)).collect(),
        }
    }
}

impl Resolve for HandlerExpr {
    type Output = abs::HandlerExpr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::HandlerExpr {
            expr: self.expr.resolve(ctx),
            with: self.handler.resolve(ctx),
        }
    }
}

impl Resolve for CasesExpr {
    type Output = abs::CasesExpr;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::CasesExpr {
            arms: self.arms.resolve(ctx),
        }
    }
}

impl Resolve for Tuple {
    type Output = abs::Tuple;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::Tuple {
            exprs: self.data.into_iter().map(|x| x.0.resolve(ctx)).collect(),
        }
    }
}

impl Resolve for LetSttm {
    type Output = abs::LetStatement;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::LetStatement {
            pattern: self.pattern.resolve(ctx),
            expr: self.expr.resolve(ctx),
        }
    }
}

impl Resolve for StatementKind {
    type Output = abs::StatementKind;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        match self {
            StatementKind::Let(x) => abs::StatementKind::Let(x.resolve(ctx)),
            StatementKind::Expr(x) => abs::StatementKind::Expr(x.resolve(ctx)),
            StatementKind::Error(_) => abs::StatementKind::Error,
        }
    }
}

impl Resolve for DoExpr {
    type Output = abs::Block;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::Block {
            statements: self.block.statements.resolve(ctx),
        }
    }
}

impl Resolve for ExprKind {
    type Output = abs::ExprKind;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        match self {
            ExprKind::Variable(x) => abs::ExprKind::Variable(x.symbol()),
            ExprKind::Constructor(x) => {
                find_constructor(x, ctx, abs::ExprKind::Constructor, abs::ExprKind::Error)
            }
            ExprKind::Function(x) => expect_function_or_effect(x, ctx),
            ExprKind::Do(x) => abs::ExprKind::Do(x.resolve(ctx)),
            ExprKind::Lambda(x) => abs::ExprKind::Lambda(x.resolve(ctx)),
            ExprKind::Application(x) => abs::ExprKind::Application(x.resolve(ctx)),
            ExprKind::Acessor(x) => abs::ExprKind::Projection(x.resolve(ctx)),
            ExprKind::Binary(x) => x.resolve(ctx),
            ExprKind::Let(x) => abs::ExprKind::Let(x.resolve(ctx)),
            ExprKind::If(x) => x.resolve(ctx),
            ExprKind::When(x) => abs::ExprKind::When(x.resolve(ctx)),
            ExprKind::Literal(x) => abs::ExprKind::Literal(Box::new(x.resolve(ctx))),
            ExprKind::Handler(x) => abs::ExprKind::Handler(x.resolve(ctx)),
            ExprKind::Cases(x) => abs::ExprKind::Cases(x.resolve(ctx)),
            ExprKind::Annotation(x) => abs::ExprKind::Annotation(x.resolve(ctx)),
            ExprKind::RecordInstance(x) => x.resolve(ctx),
            ExprKind::RecordUpdate(x) => abs::ExprKind::RecordUpdate(x.resolve(ctx)),
            ExprKind::Parenthesis(x) => x.data.0.resolve(ctx).data,
            ExprKind::Tuple(x) => abs::ExprKind::Tuple(x.resolve(ctx)),
        }
    }
}

fn expect_function_or_effect(x: Path<Lower>, ctx: &Context<'_>) -> abs::ExprKind {
    let vec: Vec<_> = (&x).into();

    match ctx.find_value(x.span.clone(), &vec) {
        Some(Item {
            item: Value::Function(qual),
            ..
        }) => abs::ExprKind::Function(qual),
        Some(Item {
            item: Value::Effect(qual),
            ..
        }) => abs::ExprKind::Effect(qual),
        Some(_) => {
            ctx.report(ResolverError {
                span: x.span,
                kind: ResolverErrorKind::ExpectedFunction,
            });
            abs::ExprKind::Error
        }
        None => abs::ExprKind::Error,
    }
}

fn find_constructor<T>(x: Path<Upper>, ctx: &Context<'_>, ok: fn(Qualified) -> T, error: T) -> T {
    let vec: Vec<_> = (&x).into();
    find_constructor_raw(x.span, &vec, ctx, ok, error)
}

fn find_constructor_raw<T>(
    span: Span,
    x: &[Symbol],
    ctx: &Context<'_>,
    ok: fn(Qualified) -> T,
    error: T,
) -> T {
    match ctx.find_value(span.clone(), x) {
        Some(Item {
            item: Value::Constructor(qual),
            ..
        }) => ok(qual),
        Some(_) => {
            ctx.report(ResolverError {
                span,
                kind: ResolverErrorKind::ExpectedConstructor,
            });
            error
        }
        None => error,
    }
}

impl Resolve for Binder {
    type Output = abs::Binder;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::Binder {
            pattern: self.pattern.resolve(ctx),
            ty: self.typ.resolve(ctx),
        }
    }
}

impl Resolve for LetCase {
    type Output = abs::LetCase;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::LetCase {
            pattern: self.arm.resolve(ctx),
        }
    }
}

impl Resolve for LetMode {
    type Output = abs::LetMode;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        match self {
            LetMode::Body(_, expr) => abs::LetMode {
                cases: vec![abs::LetCase {
                    pattern: abs::PatternArm {
                        pattern: vec![],
                        expr: expr.resolve(ctx),
                        guard: None,
                    },
                }],
            },
            LetMode::Cases(cases) => abs::LetMode {
                cases: cases.resolve(ctx),
            },
        }
    }
}

impl Resolve for LetDecl {
    type Output = abs::LetDecl;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::LetDecl {
            visibility: self.visibility.resolve(ctx),
            name: self.name.symbol(),
            binders: self.binders.resolve(ctx),
            ret: self.ret.map(|x| (x.1.resolve(ctx), x.2.resolve(ctx))),
            body: self.body.resolve(ctx),
        }
    }
}

impl Resolve for Constructor {
    type Output = abs::Constructor;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::Constructor {
            name: self.name.symbol(),
            args: self.args.resolve(ctx),
        }
    }
}

impl Resolve for SumDecl {
    type Output = abs::SumDecl;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::SumDecl {
            constructors: self.constructors.resolve(ctx),
        }
    }
}

impl Resolve for Field {
    type Output = (Symbol, abs::Type);

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        (self.name.symbol(), self.ty.resolve(ctx))
    }
}

impl Resolve for RecordDecl {
    type Output = abs::RecordDecl;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::RecordDecl {
            fields: self.fields.into_iter().map(|x| x.0.resolve(ctx)).collect(),
        }
    }
}

impl Resolve for TypeDef {
    type Output = abs::TypeDef;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        match self {
            TypeDef::Sum(sum) => abs::TypeDef::Sum(sum.resolve(ctx)),
            TypeDef::Record(rec) => abs::TypeDef::Record(rec.resolve(ctx)),
            TypeDef::Synonym(sym) => abs::TypeDef::Synonym(sym.resolve(ctx)),
        }
    }
}

impl Resolve for TypeDecl {
    type Output = abs::TypeDecl;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::TypeDecl {
            visibility: self.visibility.resolve(ctx),
            name: self.name.symbol(),
            binders: self.binders.resolve(ctx),
            def: if let Some(res) = self.def {
                res.1.resolve(ctx)
            } else {
                abs::TypeDef::Abstract
            },
        }
    }
}

impl Resolve for ModuleInline {
    type Output = Vec<abs::TopLevelDecl>;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        self.top_levels
            .into_iter()
            .filter_map(|x| x.0.resolve(ctx))
            .collect()
    }
}

impl Resolve for ModuleDecl {
    type Output = abs::ModuleDecl;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::ModuleDecl {
            visibility: self.visibility.resolve(ctx),
            name: self.name.symbol(),
            decls: self.part.resolve(ctx),
        }
    }
}

impl Resolve for Visibility {
    type Output = abs::Visibility;

    fn resolve(self, _: &mut Context) -> Self::Output {
        match self {
            Visibility::Public(_) => abs::Visibility::Public,
            Visibility::Private => abs::Visibility::Private,
        }
    }
}

impl Resolve for TypeBinder {
    type Output = abs::TypeBinder;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        match self {
            TypeBinder::Implicit(name) => abs::TypeBinder::Implicit(name.symbol()),
            TypeBinder::Explicit(binder) => {
                abs::TypeBinder::Explicit(binder.data.name.symbol(), binder.data.kind.resolve(ctx))
            }
        }
    }
}

impl Resolve for EffectField {
    type Output = abs::EffectField;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::EffectField {
            visibility: self.visibility.resolve(ctx),
            name: self.name.symbol(),
            args: self.args.resolve(ctx),
            ty: self.ret.resolve(ctx),
        }
    }
}

impl Resolve for EffectDecl {
    type Output = abs::EffectDecl;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::EffectDecl {
            visibility: self.visibility.resolve(ctx),
            name: self.name.symbol(),
            binders: self.binders.resolve(ctx),
            fields: self.fields.into_iter().map(|x| x.0.resolve(ctx)).collect(),
        }
    }
}

impl Resolve for TopLevel {
    type Output = Option<abs::TopLevelDecl>;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        match self {
            TopLevel::Let(let_) => Some(abs::TopLevelDecl::Let(let_.resolve(ctx))),
            TopLevel::Type(typ) => Some(abs::TopLevelDecl::Type(typ.resolve(ctx))),
            TopLevel::Module(module) => Some(abs::TopLevelDecl::Module(module.resolve(ctx))),
            TopLevel::Effect(effect) => Some(abs::TopLevelDecl::Effect(effect.resolve(ctx))),
            TopLevel::Error(_) => None,
            TopLevel::Use(_) => None,
        }
    }
}

impl Resolve for Program {
    type Output = abs::Module;

    fn resolve(self, ctx: &mut Context) -> Self::Output {
        abs::Module {
            decls: self
                .top_levels
                .into_iter()
                .filter_map(|x| x.resolve(ctx))
                .collect(),
        }
    }
}
