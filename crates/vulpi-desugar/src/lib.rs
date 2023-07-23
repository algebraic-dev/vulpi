use vulpi_location::Spanned;

use vulpi_syntax::concrete::LetMode;
use vulpi_syntax::{concrete, r#abstract as abs};

pub mod error;

pub struct DesugarCtx {
    program: abs::Program,
}

impl Default for DesugarCtx {
    fn default() -> Self {
        DesugarCtx {
            program: abs::Program {
                uses: Default::default(),
                types: Default::default(),
                lets: Default::default(),
            },
        }
    }
}

trait Desugar {
    type Output;
    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output;
}

impl<T: Desugar> Desugar for Spanned<T> {
    type Output = Spanned<T::Output>;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        Spanned {
            range: self.range.clone(),
            data: self.data.desugar(ctx),
        }
    }
}

impl<T: Desugar> Desugar for Option<T> {
    type Output = Option<T::Output>;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        self.as_ref().map(|x| x.desugar(ctx))
    }
}

impl<T: Desugar> Desugar for &T {
    type Output = T::Output;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        (*self).desugar(ctx)
    }
}

impl<T: Desugar> Desugar for Vec<T> {
    type Output = Vec<T::Output>;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        self.iter().map(|x| x.desugar(ctx)).collect()
    }
}

impl Desugar for concrete::Ident {
    type Output = abs::Ident;

    fn desugar(&self, _: &mut DesugarCtx) -> Self::Output {
        abs::Ident(self.0.data.data.clone(), self.0.range.clone())
    }
}

impl Desugar for concrete::Upper {
    type Output = abs::Ident;

    fn desugar(&self, _: &mut DesugarCtx) -> Self::Output {
        abs::Ident(self.0.data.data.clone(), self.0.range.clone())
    }
}

impl Desugar for concrete::Lower {
    type Output = abs::Ident;

    fn desugar(&self, _: &mut DesugarCtx) -> Self::Output {
        abs::Ident(self.0.data.data.clone(), self.0.range.clone())
    }
}

impl Desugar for concrete::Effects {
    type Output = abs::Effects;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::Effects {
            effects: self.effects.iter().map(|x| x.0.desugar(ctx)).collect(),
        }
    }
}

impl Desugar for concrete::TypeArrow {
    type Output = abs::TypeArrow;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::TypeArrow {
            left: Box::new(self.left.desugar(ctx)),
            effects: self.effects.desugar(ctx).unwrap_or_default(),
            right: Box::new(self.right.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::TypeApplication {
    type Output = abs::TypeApplication;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::TypeApplication {
            left: Box::new(self.func.desugar(ctx)),
            right: self.args.iter().map(|x| x.desugar(ctx)).collect(),
        }
    }
}

impl Desugar for concrete::TypeForall {
    type Output = abs::TypeForall;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::TypeForall {
            params: self.params.iter().map(|x| x.desugar(ctx)).collect(),
            body: Box::new(self.body.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::TypeKind {
    type Output = abs::TypeKind;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        use abs::TypeKind as AbstractType;
        use concrete::TypeKind as ConcreteType;

        match self {
            ConcreteType::Parenthesis(s) => s.data.desugar(ctx).data,
            ConcreteType::Upper(s) => AbstractType::Upper(s.desugar(ctx)),
            ConcreteType::Lower(s) => AbstractType::Lower(s.desugar(ctx)),
            ConcreteType::Arrow(s) => AbstractType::Arrow(s.desugar(ctx)),
            ConcreteType::Application(s) => AbstractType::Application(s.desugar(ctx)),
            ConcreteType::Forall(s) => AbstractType::Forall(s.desugar(ctx)),
            ConcreteType::Unit(_) => AbstractType::Unit,
        }
    }
}

impl Desugar for concrete::LiteralKind {
    type Output = abs::LiteralKind;

    fn desugar(&self, _: &mut DesugarCtx) -> Self::Output {
        use abs::LiteralKind as AbstractLit;
        use concrete::LiteralKind as ConcreteLit;

        match self {
            ConcreteLit::String(s) => {
                AbstractLit::String(abs::Ident(s.data.data.clone(), s.range.clone()))
            }
            ConcreteLit::Integer(s) => {
                AbstractLit::Integer(abs::Ident(s.data.data.clone(), s.range.clone()))
            }
            ConcreteLit::Float(s) => {
                AbstractLit::Float(abs::Ident(s.data.data.clone(), s.range.clone()))
            }
            ConcreteLit::Char(s) => {
                AbstractLit::Char(abs::Ident(s.data.data.clone(), s.range.clone()))
            }
            ConcreteLit::Unit(_) => AbstractLit::Unit,
        }
    }
}

impl<T: Desugar<Output = abs::Ident>> Desugar for concrete::Path<T> {
    type Output = abs::Qualified;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::Qualified {
            segments: self.segments.iter().map(|x| x.0.desugar(ctx)).collect(),
            last: self.last.desugar(ctx),
            range: self.span.clone(),
        }
    }
}

impl Desugar for concrete::PatAnnotation {
    type Output = abs::PatAnnotation;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::PatAnnotation {
            pat: Box::new(self.left.desugar(ctx)),
            ty: Box::new(self.right.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::PatOr {
    type Output = abs::PatOr;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::PatOr {
            left: Box::new(self.left.desugar(ctx)),
            right: Box::new(self.right.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::PatApplication {
    type Output = abs::PatApplication;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::PatApplication {
            func: self.func.desugar(ctx),
            args: self.args.iter().map(|x| x.desugar(ctx)).collect(),
        }
    }
}

impl Desugar for concrete::PatternKind {
    type Output = abs::PatternKind;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        use abs::PatternKind as AbstractPat;
        use concrete::PatternKind as ConcretePat;

        match self {
            ConcretePat::Wildcard(_) => AbstractPat::Wildcard,
            ConcretePat::Upper(upper) => AbstractPat::Upper(upper.desugar(ctx)),
            ConcretePat::Lower(lower) => AbstractPat::Lower(lower.desugar(ctx)),
            ConcretePat::Literal(lit) => AbstractPat::Literal(lit.desugar(ctx)),
            ConcretePat::Annotation(ann) => AbstractPat::Annotation(ann.desugar(ctx)),
            ConcretePat::Or(or) => AbstractPat::Or(or.desugar(ctx)),
            ConcretePat::Application(app) => AbstractPat::Application(app.desugar(ctx)),
            ConcretePat::Parenthesis(par) => par.data.desugar(ctx).data,
        }
    }
}

impl Desugar for concrete::Binder {
    type Output = (abs::Pattern, abs::Type);

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        let pat = self.pattern.desugar(ctx);
        let typ = self.typ.desugar(ctx);

        (pat, typ)
    }
}

impl Desugar for concrete::LambdaExpr {
    type Output = Box<abs::Expr>;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        let patterns = self
            .patterns
            .iter()
            .map(|x| x.desugar(ctx))
            .collect::<Vec<_>>();

        let body = Box::new(self.expr.desugar(ctx));
        let end = body.range.end.clone();

        patterns.into_iter().fold(body, |acc, pattern| {
            Box::new(abs::Expr {
                range: pattern.range.start.clone()..end.clone(),
                data: abs::ExprKind::Lambda(abs::LambdaExpr { pattern, body: acc }),
            })
        })
    }
}

impl Desugar for concrete::ApplicationExpr {
    type Output = abs::ApplicationExpr;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::ApplicationExpr {
            func: Box::new(self.func.desugar(ctx)),
            args: self.args.iter().map(|x| x.desugar(ctx)).collect(),
        }
    }
}

impl Desugar for concrete::AcessorExpr {
    type Output = abs::AcessorExpr;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::AcessorExpr {
            expr: Box::new(self.expr.desugar(ctx)),
            field: self.field.desugar(ctx),
        }
    }
}

impl Desugar for concrete::BinaryExpr {
    type Output = abs::BinaryExpr;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::BinaryExpr {
            left: Box::new(self.left.desugar(ctx)),
            op: self.op.desugar(ctx),
            right: Box::new(self.right.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::Operator {
    type Output = Spanned<abs::Operator>;

    fn desugar(&self, _: &mut DesugarCtx) -> Self::Output {
        use abs::Operator as AbstractOp;
        use concrete::Operator as ConcreteOp;

        let (range, data) = match self {
            ConcreteOp::Add(t) => (t.range.clone(), AbstractOp::Add),
            ConcreteOp::Sub(t) => (t.range.clone(), AbstractOp::Sub),
            ConcreteOp::Mul(t) => (t.range.clone(), AbstractOp::Mul),
            ConcreteOp::Div(t) => (t.range.clone(), AbstractOp::Div),
            ConcreteOp::Rem(t) => (t.range.clone(), AbstractOp::Rem),
            ConcreteOp::And(t) => (t.range.clone(), AbstractOp::And),
            ConcreteOp::Or(t) => (t.range.clone(), AbstractOp::Or),
            ConcreteOp::Xor(t) => (t.range.clone(), AbstractOp::Xor),
            ConcreteOp::Not(t) => (t.range.clone(), AbstractOp::Not),
            ConcreteOp::Eq(t) => (t.range.clone(), AbstractOp::Eq),
            ConcreteOp::Neq(t) => (t.range.clone(), AbstractOp::Neq),
            ConcreteOp::Lt(t) => (t.range.clone(), AbstractOp::Lt),
            ConcreteOp::Gt(t) => (t.range.clone(), AbstractOp::Gt),
            ConcreteOp::Le(t) => (t.range.clone(), AbstractOp::Le),
            ConcreteOp::Ge(t) => (t.range.clone(), AbstractOp::Ge),
            ConcreteOp::Shl(t) => (t.range.clone(), AbstractOp::Shl),
            ConcreteOp::Shr(t) => (t.range.clone(), AbstractOp::Shr),
            ConcreteOp::Pipe(t) => (t.range.clone(), AbstractOp::Pipe),
        };

        Spanned { range, data }
    }
}

impl Desugar for concrete::LetExpr {
    type Output = abs::LetExpr;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::LetExpr {
            name: Box::new(self.pattern.desugar(ctx)),
            value: Box::new(self.value.desugar(ctx)),
            body: Box::new(self.body.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::IfExpr {
    type Output = abs::IfExpr;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::IfExpr {
            cond: Box::new(self.cond.desugar(ctx)),
            then: Box::new(self.then_expr.desugar(ctx)),
            else_: Box::new(self.else_expr.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::WhenArm {
    type Output = abs::WhenArm;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::WhenArm {
            pattern: Box::new(self.pattern.desugar(ctx)),
            then: Box::new(self.expr.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::WhenExpr {
    type Output = abs::WhenExpr;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::WhenExpr {
            scrutinee: Box::new(self.scrutinee.desugar(ctx)),
            arms: self.arms.iter().map(|x| x.desugar(ctx)).collect(),
        }
    }
}

impl Desugar for concrete::AnnotationExpr {
    type Output = abs::AnnotationExpr;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::AnnotationExpr {
            expr: Box::new(self.expr.desugar(ctx)),
            ty: Box::new(self.ty.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::LetSttm {
    type Output = abs::LetStmt;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::LetStmt {
            name: Box::new(self.pattern.desugar(ctx)),
            expr: Box::new(self.expr.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::StatementKind {
    type Output = abs::StatementKind;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        use abs::StatementKind as AbstractStatement;
        use concrete::StatementKind as ConcreteStatement;

        match self {
            ConcreteStatement::Let(d) => AbstractStatement::Let(d.desugar(ctx)),
            ConcreteStatement::Expr(d) => AbstractStatement::Expr(d.desugar(ctx)),
            ConcreteStatement::Error(_) => AbstractStatement::Error,
        }
    }
}

impl Desugar for concrete::DoExpr {
    type Output = abs::Block;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::Block {
            statements: self
                .block
                .statements
                .iter()
                .map(|x| x.0.desugar(ctx))
                .collect(),
        }
    }
}

impl Desugar for concrete::RecordField {
    type Output = (abs::Ident, abs::Expr);

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        (self.name.desugar(ctx), self.expr.desugar(ctx))
    }
}

impl Desugar for concrete::RecordInstance {
    type Output = abs::RecordInstance;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::RecordInstance {
            name: self.name.desugar(ctx),
            fields: self.fields.iter().map(|x| x.0.desugar(ctx)).collect(),
        }
    }
}

impl Desugar for concrete::RecordUpdate {
    type Output = abs::RecordUpdate;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::RecordUpdate {
            fields: self.fields.iter().map(|x| x.0.desugar(ctx)).collect(),
            expr: Box::new(self.expr.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::ExprKind {
    type Output = abs::ExprKind;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        use abs::ExprKind as AbstractKind;
        use concrete::ExprKind as ConcreteExpr;

        match self {
            ConcreteExpr::Lambda(d) => d.desugar(ctx).data,
            ConcreteExpr::Application(d) => AbstractKind::Application(d.desugar(ctx)),
            ConcreteExpr::Ident(d) => AbstractKind::Ident(d.desugar(ctx)),
            ConcreteExpr::Acessor(d) => AbstractKind::Acessor(d.desugar(ctx)),
            ConcreteExpr::Binary(d) => AbstractKind::Binary(d.desugar(ctx)),
            ConcreteExpr::Let(d) => AbstractKind::Let(d.desugar(ctx)),
            ConcreteExpr::If(d) => AbstractKind::If(d.desugar(ctx)),
            ConcreteExpr::When(d) => AbstractKind::When(d.desugar(ctx)),
            ConcreteExpr::Annotation(d) => AbstractKind::Annotation(d.desugar(ctx)),
            ConcreteExpr::Do(d) => AbstractKind::Block(d.desugar(ctx)),
            ConcreteExpr::Literal(d) => AbstractKind::Literal(d.desugar(ctx)),
            ConcreteExpr::RecordInstance(d) => AbstractKind::RecordInstance(d.desugar(ctx)),
            ConcreteExpr::RecordUpdate(d) => AbstractKind::RecordUpdate(d.desugar(ctx)),
            ConcreteExpr::Parenthesis(d) => d.data.desugar(ctx).data,
        }
    }
}

impl Desugar for concrete::LetCase {
    type Output = abs::LetCase;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::LetCase {
            patterns: self.patterns.iter().map(|x| x.0.desugar(ctx)).collect(),
            body: Box::new(self.expr.desugar(ctx)),
            range: self.pipe.range.start.clone()..self.expr.range.end.clone(),
        }
    }
}

impl Desugar for concrete::LetDecl {
    type Output = abs::LetDecl;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        let mut clauses = Vec::new();

        match &self.body {
            LetMode::Body(_, body) => {
                clauses.push(abs::LetCase {
                    patterns: vec![],
                    body: Box::new(body.desugar(ctx)),
                    range: body.range.clone(),
                });
            }
            LetMode::Cases(cases) => {
                for case in cases {
                    clauses.push(case.desugar(ctx));
                }
            }
        }

        abs::LetDecl {
            name: self.name.desugar(ctx),
            params: self.binders.iter().map(|x| x.desugar(ctx)).collect(),
            cases: clauses,
            ret: self.typ.as_ref().map(|x| x.1.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::Constructor {
    type Output = abs::Variant;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::Variant {
            name: self.name.desugar(ctx),
            args: self.args.iter().map(|x| x.desugar(ctx)).collect(),
        }
    }
}

impl Desugar for concrete::SumDecl {
    type Output = abs::EnumDecl;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::EnumDecl {
            variants: self.constructors.iter().map(|x| x.desugar(ctx)).collect(),
        }
    }
}

impl Desugar for concrete::Field {
    type Output = abs::Field;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::Field {
            name: self.name.desugar(ctx),
            ty: Box::new(self.ty.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::RecordDecl {
    type Output = abs::RecordDecl;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::RecordDecl {
            fields: self.fields.iter().map(|x| x.0.desugar(ctx)).collect(),
        }
    }
}

impl Desugar for concrete::TypeDef {
    type Output = abs::TypeDef;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        use abs::TypeDef as AbstractTypeDef;
        use concrete::TypeDef as ConcreteTypeDef;

        match self {
            ConcreteTypeDef::Sum(s) => AbstractTypeDef::Enum(s.desugar(ctx)),
            ConcreteTypeDef::Record(s) => AbstractTypeDef::Record(s.desugar(ctx)),
            ConcreteTypeDef::Synonym(s) => AbstractTypeDef::Synonym(s.desugar(ctx)),
        }
    }
}

impl Desugar for concrete::TypeDecl {
    type Output = abs::TypeDecl;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::TypeDecl {
            id: None,
            name: self.name.desugar(ctx),
            params: self.binders.iter().map(|x| x.desugar(ctx)).collect(),
            def: self.def.desugar(ctx),
        }
    }
}

impl Desugar for concrete::UseAlias {
    type Output = abs::Ident;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        self.alias.desugar(ctx)
    }
}

impl Desugar for concrete::UseDecl {
    type Output = abs::UseDecl;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::UseDecl {
            path: self.path.desugar(ctx),
            alias: self.alias.desugar(ctx),
        }
    }
}

impl Desugar for concrete::Program {
    type Output = ();

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        for top in &self.top_levels {
            match top {
                concrete::TopLevel::Let(let_) => {
                    let desugared = let_.desugar(ctx);
                    ctx.program.lets.push(desugared);
                }
                concrete::TopLevel::Type(type_) => {
                    let desugared = type_.desugar(ctx);
                    ctx.program.types.push(desugared);
                }
                concrete::TopLevel::Use(use_) => {
                    let desugared = use_.desugar(ctx);
                    ctx.program.uses.push(desugared);
                }
                _ => (),
            }
        }
    }
}

pub fn desugar(concrete: concrete::Program) -> abs::Program {
    let mut ctx = DesugarCtx::default();
    concrete.desugar(&mut ctx);

    ctx.program
}
