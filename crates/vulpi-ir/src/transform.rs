//! This module compiles a TypedTree to a Vulpi IR tree called Lambda. This is the first step in
//! lowering the AST to a form that is easier to work with for code generation.

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_syntax::{elaborated, lambda, r#abstract::Qualified};
use vulpi_typer::{Type, Real};

pub trait Transform {
    type Out;

    fn transform(self) -> Self::Out;
}

impl<T: Transform> Transform for Box<T> {
    type Out = Box<T::Out>;

    fn transform(self) -> Self::Out {
        Box::new((*self).transform())
    }
}

impl<T: Transform> Transform for Vec<T> {
    type Out = Vec<T::Out>;

    fn transform(self) -> Self::Out {
        self.into_iter().map(|x| x.transform()).collect()
    }
}

impl<T: Transform> Transform for Option<T> {
    type Out = Option<T::Out>;

    fn transform(self) -> Self::Out {
        self.map(|x| x.transform())
    }
}


impl<T: Transform> Transform for HashMap<Qualified, T> {
    type Out = HashMap<Qualified, T::Out>;

    fn transform(self) -> Self::Out {
        self.into_iter().map(|(k, v)| (k, v.transform())).collect()
    }

}

impl Transform for elaborated::LiteralKind {
    type Out = lambda::LiteralKind;

    fn transform(self) -> Self::Out {
        match self {
            elaborated::LiteralKind::String(x) => lambda::LiteralKind::String(x),
            elaborated::LiteralKind::Integer(x) => lambda::LiteralKind::Integer(x),
            elaborated::LiteralKind::Float(x) => lambda::LiteralKind::Float(x),
            elaborated::LiteralKind::Char(x) => lambda::LiteralKind::Char(x),
            elaborated::LiteralKind::Unit => lambda::LiteralKind::Unit,
        }
    }
}

impl Transform for elaborated::PatternKind {
    type Out = lambda::PatternKind;

    fn transform(self) -> Self::Out {
        use lambda::PatternKind::*;
        match self {
            elaborated::PatternKind::Wildcard => Wildcard,
            elaborated::PatternKind::Variable(x) => Variable(x),
            elaborated::PatternKind::Literal(x) => Literal(x.transform()),
            elaborated::PatternKind::Application(x) => Application(x.func, x.args.transform()),
            elaborated::PatternKind::Tuple(x) => Tuple(x.transform()),
            elaborated::PatternKind::Error => unreachable!(),
        }
    }
}

impl Transform for elaborated::Statement<Type<Real>> {
    type Out = lambda::Statement;

    fn transform(self) -> Self::Out {
        use lambda::Statement::*;
        match self {
            elaborated::Statement::Let(x) => Let(x.pattern.transform(), x.expr.transform()),
            elaborated::Statement::Expr(x) => Expr(x.transform()),
            elaborated::Statement::Error => unreachable!(),
        }
    }
}

impl Transform for elaborated::PatternArm<Type<Real>> {
    type Out = lambda::PatternArm;

    fn transform(self) -> Self::Out {
        lambda::PatternArm {
            patterns: self.patterns.transform(),
            expr: self.expr.transform(),
            guard: self.guard.transform(),
        }
    }
}

impl Transform for (Symbol, Box<elaborated::ExprKind<Type<Real>>>) {
    type Out = (Symbol, Box<lambda::ExprKind>);

    fn transform(self) -> Self::Out {
        (self.0, self.1.transform())
    }
}

impl Transform for elaborated::ExprKind<Type<Real>> {
    type Out = lambda::ExprKind;

    fn transform(self) -> Self::Out {
        use lambda::ExprKind::*;

        match self {
            elaborated::ExprKind::Lambda(x) => Lambda(x.param.transform(), x.body.transform()),
            elaborated::ExprKind::Application(x) => {
                Application(x.func.transform(), x.args.transform())
            }
            elaborated::ExprKind::Variable(x) => Variable(x),
            elaborated::ExprKind::Constructor(x, y) => Constructor(x, y),
            elaborated::ExprKind::Function(x, _) => Function(x),
            elaborated::ExprKind::Projection(x) => Projection(x.field, x.expr.transform()),
            elaborated::ExprKind::Let(x) => Let(x.pattern.transform(), x.body.transform(), x.value.transform()),
            elaborated::ExprKind::When(x) => When(x.scrutinee.transform(), x.arms.transform()),
            elaborated::ExprKind::Do(x) => Do(x.transform()),
            elaborated::ExprKind::Literal(x) => Literal(x.transform()),
            elaborated::ExprKind::RecordInstance(x) => RecordInstance(x.name, x.fields.transform()),
            elaborated::ExprKind::RecordUpdate(x) => RecordUpdate(x.qualified, x.expr.transform(), x.fields.transform()),
            elaborated::ExprKind::Tuple(x) => Tuple(x.exprs.transform()),
            elaborated::ExprKind::Error => todo!(),
        }
    }
}

impl Transform for elaborated::LetDecl<Type<Real>> {
    type Out = lambda::LetDecl;

    fn transform(self) -> Self::Out {
        lambda::LetDecl {
            binders: self.binders.into_iter().map(|x| x.0).collect::<Vec<_>>().transform(),
            body: self.body.transform(),
        }
    }
}

impl Transform for elaborated::ExternalDecl<Type<Real>> {
    type Out = lambda::ExternalDecl;

    fn transform(self) -> Self::Out {
        lambda::ExternalDecl {
            binding: self.binding,
        }
    }
}

impl Transform for elaborated::TypeDecl {
    type Out = lambda::TypeDecl;

    fn transform(self) -> Self::Out {
        use lambda::TypeDecl::*;
        match self {
            elaborated::TypeDecl::Abstract => Abstract,
            elaborated::TypeDecl::Enum(x) => Enum(x),
            elaborated::TypeDecl::Record(x) => Record(x),
        }
    }
}

impl Transform for elaborated::Program<Type<Real>> {
    type Out = lambda::Program;

    fn transform(self) -> Self::Out {
        lambda::Program {
            lets: self.lets.transform(),
            types: self.types.transform(),
            externals: self.externals.transform(),
        }
    }
}