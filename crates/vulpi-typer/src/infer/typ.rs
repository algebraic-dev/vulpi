//! Inference of types and kinds.

use std::rc::Rc;

use vulpi_syntax::resolved::*;

use super::Infer;
use crate::types::{Kind, KindType, Mono, Type};
use crate::unify::unify_kinds;

impl Infer for TypeArrow {
    type Out = (Kind, Type);

    fn infer(&self, env: crate::context::Env) -> Self::Out {
        let (l_kind, l_type) = self.left.infer(env.clone());
        let (r_kind, r_type) = self.right.infer(env.clone());

        unify_kinds(env.clone(), l_kind, Rc::new(KindType::Star));
        unify_kinds(env, r_kind, Rc::new(KindType::Star));

        (
            Kind::new(KindType::Star),
            Type::new(Mono::Function(l_type, r_type)),
        )
    }
}

impl Infer for TypeApplication {
    type Out = (Kind, Type);

    fn infer(&self, env: crate::context::Env) -> Self::Out {
        let (mut kind, mut l_type) = self.fun.infer(env.clone());

        for r in &self.args {
            env.set_location(r.range.clone());

            let (r_kind, r_type) = r.infer(env.clone());

            match &*kind {
                KindType::Fun(left, right) => {
                    unify_kinds(env.clone(), left.clone(), r_kind);
                    kind = right.clone();
                }
                _ => {
                    env.report(crate::error::TypeErrorKind::CannotApplyType);
                    return (Rc::new(KindType::Error), Type::new(Mono::Error));
                }
            }

            l_type = Type::new(Mono::Application(l_type, r_type));
        }

        (kind, l_type)
    }
}

impl Infer for TypeForall {
    type Out = (Kind, Type);

    fn infer(&self, env: crate::context::Env) -> Self::Out {
        env.report(crate::error::TypeErrorKind::CannotInferForall);
        (Rc::new(KindType::Error), Type::new(Mono::Error))
    }
}

impl Infer for TypeKind {
    type Out = (Kind, Type);

    fn infer(&self, env: crate::context::Env) -> Self::Out {
        match self {
            TypeKind::Upper(upper) => {
                if let Some(rep) = env.get_global_type(upper.canonical, &upper.last) {
                    let typ = Type::new(Mono::Variable(upper.canonical, upper.last.clone()));
                    (rep.kind, typ)
                } else {
                    (Rc::new(KindType::Error), Type::new(Mono::Error))
                }
            }
            TypeKind::Lower(l) => {
                if let Some((kind, gen)) = env.type_variables.get(&l.data) {
                    (
                        kind.clone(),
                        Type::new(Mono::Generalized(*gen, l.data.clone())),
                    )
                } else {
                    env.report(crate::error::TypeErrorKind::CannotFindTypeVariable(
                        l.data.clone(),
                    ));
                    (Rc::new(KindType::Error), Type::new(Mono::Error))
                }
            }
            TypeKind::Arrow(t) => t.infer(env),
            TypeKind::Application(a) => a.infer(env),
            TypeKind::Forall(f) => f.infer(env),
            TypeKind::Unit => (Rc::new(KindType::Star), Type::new(Mono::Unit)),
            TypeKind::Error => (Rc::new(KindType::Error), Type::new(Mono::Error)),
        }
    }
}
