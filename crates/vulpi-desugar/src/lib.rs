use vulpi_location::Spanned;
use vulpi_storage::interner::Symbol;
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

impl<T: Desugar> Desugar for Option<T> {
    type Output = Option<T::Output>;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        self.as_ref().map(|x| x.desugar(ctx))
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
            ConcreteType::Unit(r) => AbstractType::Upper(abs::Qualified {
                segments: Vec::new(),
                last: abs::Ident(Symbol::intern("Unit"), r.range.clone()),
                range: r.range.clone(),
            }),
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
    type Output = abs::LambdaExpr;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::LambdaExpr {
            pattern: self.patterns.iter().map(|x| x.desugar(ctx)).collect(),
            body: Box::new(self.expr.desugar(ctx)),
        }
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
    type Output = abs::Operator;

    fn desugar(&self, _: &mut DesugarCtx) -> Self::Output {
        use abs::Operator as AbstractOp;
        use concrete::Operator as ConcreteOp;

        match self {
            ConcreteOp::Add(_) => AbstractOp::Add,
            ConcreteOp::Sub(_) => AbstractOp::Sub,
            ConcreteOp::Mul(_) => AbstractOp::Mul,
            ConcreteOp::Div(_) => AbstractOp::Div,
            ConcreteOp::Rem(_) => AbstractOp::Rem,
            ConcreteOp::And(_) => AbstractOp::And,
            ConcreteOp::Or(_) => AbstractOp::Or,
            ConcreteOp::Xor(_) => AbstractOp::Xor,
            ConcreteOp::Not(_) => AbstractOp::Not,
            ConcreteOp::Eq(_) => AbstractOp::Eq,
            ConcreteOp::Neq(_) => AbstractOp::Neq,
            ConcreteOp::Lt(_) => AbstractOp::Lt,
            ConcreteOp::Gt(_) => AbstractOp::Gt,
            ConcreteOp::Le(_) => AbstractOp::Le,
            ConcreteOp::Ge(_) => AbstractOp::Ge,
            ConcreteOp::Shl(_) => AbstractOp::Shl,
            ConcreteOp::Shr(_) => AbstractOp::Shr,
            ConcreteOp::Pipe(_) => AbstractOp::Pipe,
        }
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

impl Desugar for concrete::ExprKind {
    type Output = abs::ExprKind;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        use abs::ExprKind as AbstractKind;
        use concrete::ExprKind as ConcreteExpr;

        match self {
            ConcreteExpr::Lambda(d) => AbstractKind::Lambda(d.desugar(ctx)),
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
            ConcreteExpr::Parenthesis(d) => d.data.desugar(ctx).data,
        }
    }
}

impl Desugar for concrete::LetCase {
    type Output = abs::LetCase;

    fn desugar(&self, ctx: &mut DesugarCtx) -> Self::Output {
        abs::LetCase {
            patterns: self.patterns.iter().map(|x| x.desugar(ctx)).collect(),
            body: Box::new(self.expr.desugar(ctx)),
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
            fields: self.fields.iter().map(|x| x.desugar(ctx)).collect(),
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
