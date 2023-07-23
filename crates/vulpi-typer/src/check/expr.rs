use std::rc::Rc;

use crate::{context::Env, infer::Infer, types::Mono};
use vulpi_location::Spanned;
use vulpi_syntax::resolved::{ExprKind, StatementKind};

use super::Check;

impl<T: Check> Check for Spanned<T> {
    type Out = T::Out;

    fn check(&self, typ: crate::types::Type, env: crate::context::Env) -> Self::Out {
        env.set_location(self.range.clone());
        self.data.check(typ, env)
    }
}

impl Check for StatementKind {
    type Out = Env;

    fn check(&self, typ: crate::types::Type, env: crate::context::Env) -> Self::Out {
        let (env, inferred) = self.infer(env);
        crate::unify::unify(env.clone(), typ, inferred);
        env
    }
}

impl Check for ExprKind {
    type Out = ();

    fn check(&self, typ: crate::types::Type, mut env: crate::context::Env) -> Self::Out {
        match self {
            ExprKind::Block(block) => {
                for (i, stmt) in block.statements.iter().enumerate() {
                    if i == block.statements.len() - 1 {
                        stmt.check(Rc::new(Mono::Unit), env.clone());
                    } else {
                        (env, _) = stmt.infer(env.clone());
                    }
                }
            }
            _ => {
                let inferred = self.infer(env.clone());
                crate::unify::unify(env, typ, inferred);
            }
        }
    }
}
