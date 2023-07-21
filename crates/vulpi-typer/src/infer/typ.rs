use vulpi_syntax::resolved::*;

use crate::types::{Kind, Mono, Type};

use super::Infer;

impl Infer for TypeKind {
    type Out = Kind;

    fn infer(&self, env: crate::context::Env) -> (Self::Out, Type) {
        match self {
            TypeKind::Upper(upper) => {
                if let Some(kind) = env.get_global_type(upper.canonical, &upper.last) {
                    let typ = Type::new(Mono::Variable(upper.canonical, upper.last.clone()));
                    (kind, typ)
                } else {
                    (Kind::Error, Type::new(Mono::Error))
                }
            }
            TypeKind::Lower(l) => {
                if let Some((kind, gen)) = env.type_variables.get(&l.data) {
                    (kind.clone(), Type::new(Mono::Generalized(*gen)))
                } else {
                    env.report(crate::error::TypeErrorKind::CannotFindTypeVariable(
                        l.data.clone(),
                    ));
                    (Kind::Error, Type::new(Mono::Error))
                }
            }
            TypeKind::Arrow(TypeArrow { left, right, .. }) => {
                // TODO: Both sides have to have Star side
                let (_, l_type) = left.infer(env.clone());
                let (_, r_type) = right.infer(env);
                (Kind::Star, Type::new(Mono::Function(l_type, r_type)))
            }
            TypeKind::Application(TypeApplication { fun, args }) => {
                // TODO: Check kind
                let (mut kind, mut l_type) = fun.infer(env.clone());

                for r in args {
                    let (_, r_type) = r.infer(env.clone());

                    match kind {
                        Kind::Fun(_, right) => {
                            // Unify kinds
                            kind = *right;
                        }
                        _ => {
                            env.report(crate::error::TypeErrorKind::CannotApplyType);
                            return (Kind::Error, Type::new(Mono::Error));
                        }
                    }

                    l_type = Type::new(Mono::Application(l_type, r_type));
                }

                (kind, l_type)
            }
            TypeKind::Forall(_) => {
                env.report(crate::error::TypeErrorKind::CannotInferForall);
                (Kind::Error, Type::new(Mono::Error))
            }
            TypeKind::Error => (Kind::Error, Type::new(Mono::Error)),
        }
    }
}
