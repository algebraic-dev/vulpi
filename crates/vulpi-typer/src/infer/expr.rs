use vulpi_storage::{
    id::{Id, Namespace},
    interner::Symbol,
};

use vulpi_syntax::resolved::{
    AcessorExpr, AnnotationExpr, ApplicationExpr, Block, ExprKind, LambdaExpr, LetExpr,
    LiteralKind, WhenExpr,
};

use crate::{
    error::TypeErrorKind,
    types::{Mono, Type},
    unify,
};

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

        for arg in &self.args {
            env.set_location(arg.range.clone());

            let arg = arg.infer(env.clone());

            if let Mono::Function(left, right) = &*func_ty {
                unify::unify(env.clone(), left.clone(), arg);
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

            let body = arm.then.infer(env.clone());

            unify::unify(env, body, ret_type.clone());
        }

        let scrutinee_typ = self.scrutinee.infer(env.clone());

        unify::unify(env, scrutinee_typ, ret_type.clone());

        ret_type
    }
}

impl Infer for AnnotationExpr {
    type Out = Type;

    fn infer(&self, env: crate::context::Env) -> Self::Out {
        let (_, ty) = self.ty.infer(env.clone());
        let expr_ty = self.expr.infer(env.clone());

        unify::unify(env, ty.clone(), expr_ty);

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
            LiteralKind::Unit(_) => Type::new(Mono::Unit),
            LiteralKind::Error => Type::new(Mono::Error),
        }
    }
}

impl Infer for LetExpr {
    type Out = Type;

    fn infer(&self, mut env: crate::context::Env) -> Self::Out {
        let value = self.value.infer(env.clone());

        let (bindings, pat) = self.name.infer(env.clone());

        unify::unify(env.clone(), value, pat);

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
