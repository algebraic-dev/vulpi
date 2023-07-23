use vulpi_storage::{
    id::{Id, Namespace},
    interner::Symbol,
};

use vulpi_syntax::resolved::{
    AcessorExpr, AnnotationExpr, ApplicationExpr, Block, ExprKind, LambdaExpr, LetExpr,
    LiteralKind, WhenExpr,
};

use crate::check::Check;

use crate::error::TypeErrorKind;
use crate::types::{Mono, Type};
use crate::unify;

use super::Infer;

fn get_type_name(typ: Type) -> Option<(Id<Namespace>, Symbol)> {
    match &*typ {
        Mono::Variable(namespace, name) => Some((*namespace, name.clone())),
        Mono::Application(left, _) => get_type_name(left.clone()),
        _ => None,
    }
}

impl Infer for LambdaExpr {
    type Out = Type;

    fn infer(&self, mut env: crate::context::Env) -> Self::Out {
        let (bindings, typ) = self.pattern.infer(env.clone());

        for (k, t) in bindings {
            env.add_variable(k, t.into());
        }

        let body = self.body.infer(env.clone());

        Type::new(Mono::Function(typ, body))
    }
}

impl Infer for ApplicationExpr {
    type Out = Type;

    fn infer(&self, env: crate::context::Env) -> Self::Out {
        let mut func_ty = self.func.infer(env.clone());

        if self.args.len() > func_ty.clone().arity() {
            env.report(TypeErrorKind::WrongArity(func_ty.arity(), self.args.len()));
            return Type::new(Mono::Error);
        }

        for arg in &self.args {
            env.set_location(arg.range.clone());

            if let Mono::Function(left, right) = &*func_ty.deref() {
                arg.check(left.clone(), env.clone());
                func_ty = right.clone();
            } else {
                env.report(TypeErrorKind::CannotApplyType);
                return Type::new(Mono::Error);
            }
        }

        func_ty
    }
}

impl Infer for AcessorExpr {
    type Out = Type;

    fn infer(&self, mut env: crate::context::Env) -> Self::Out {
        let expr_ty = self.expr.infer(env.clone());

        if let Some((namespace, name)) = get_type_name(expr_ty) {
            let scheme = env
                .modules
                .borrow_mut()
                .get_field(namespace, &name)
                .unwrap()
                .clone();

            env.instantiate(scheme)
        } else {
            env.report(TypeErrorKind::CannotAccessType);
            Type::new(Mono::Error)
        }
    }
}

impl Infer for WhenExpr {
    type Out = Type;

    fn infer(&self, mut env: crate::context::Env) -> Self::Out {
        let typ = env.new_hole();
        let ret_type = env.new_hole();

        for arm in &self.arms {
            env.set_location(arm.pattern.range.clone());

            let (bindings, pat) = arm.pattern.infer(env.clone());

            unify::unify(env.clone(), pat, typ.clone());

            let mut env = env.clone();

            for (k, t) in bindings {
                env.add_variable(k, t.into());
            }

            arm.then.check(ret_type.clone(), env);
        }

        self.scrutinee.check(typ, env);

        ret_type
    }
}

impl Infer for AnnotationExpr {
    type Out = Type;

    fn infer(&self, env: crate::context::Env) -> Self::Out {
        let (_, ty) = self.ty.infer(env.clone());
        self.expr.check(ty.clone(), env);
        ty
    }
}

impl Infer for Block {
    type Out = Type;

    fn infer(&self, mut env: crate::context::Env) -> Self::Out {
        let mut typ = Type::new(Mono::Unit);

        for expr in &self.statements {
            env.set_location(expr.range.clone());
            (env, typ) = expr.infer(env.clone());
        }

        typ
    }
}

impl Infer for LiteralKind {
    type Out = Type;

    fn infer(&self, _env: crate::context::Env) -> Self::Out {
        match self {
            LiteralKind::String(_, t)
            | LiteralKind::Integer(_, t)
            | LiteralKind::Char(_, t)
            | LiteralKind::Float(_, t) => Type::new(Mono::Variable(t.canonical, t.last.clone())),
            LiteralKind::Unit => Type::new(Mono::Unit),
            LiteralKind::Error => Type::new(Mono::Error),
        }
    }
}

impl Infer for LetExpr {
    type Out = Type;

    fn infer(&self, mut env: crate::context::Env) -> Self::Out {
        let (bindings, pat_ty) = self.name.infer(env.clone());

        self.value.check(pat_ty, env.clone());

        for (k, t) in bindings {
            env.add_variable(k, t.into());
        }

        self.body.infer(env)
    }
}

impl Infer for ExprKind {
    type Out = Type;

    fn infer(&self, mut env: crate::context::Env) -> Self::Out {
        match self {
            ExprKind::Variable(var) => {
                let var = var.clone();
                if let Some(typ) = env.get_variable(var.data.clone()) {
                    env.instantiate(typ.clone())
                } else {
                    env.report(TypeErrorKind::UnboundVariable(var.data));
                    Type::new(Mono::Error)
                }
            }
            ExprKind::Function(f) => {
                let scheme = env
                    .modules
                    .borrow_mut()
                    .get_let(f.canonical, &f.last)
                    .unwrap()
                    .typ
                    .clone();

                env.instantiate(scheme)
            }
            ExprKind::Constructor(cons) => {
                let scheme = env
                    .modules
                    .borrow_mut()
                    .get_cons(cons.canonical, &cons.last)
                    .unwrap()
                    .typ
                    .clone();

                env.instantiate(scheme)
            }
            ExprKind::Lambda(lambda) => lambda.infer(env),
            ExprKind::Application(app) => app.infer(env),
            ExprKind::Acessor(accessor) => accessor.infer(env),
            ExprKind::Let(let_) => let_.infer(env),
            ExprKind::When(when) => when.infer(env),
            ExprKind::Annotation(annot) => annot.infer(env),
            ExprKind::Block(block) => block.infer(env),
            ExprKind::Literal(lit) => lit.infer(env),
            ExprKind::Error => Type::new(Mono::Error),
        }
    }
}
