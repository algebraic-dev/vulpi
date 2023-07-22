use vulpi_syntax::resolved::StatementKind;

use crate::{
    context::Env,
    types::{Mono, Type},
    unify,
};

use super::Infer;

impl Infer for StatementKind {
    type Out = (Env, Type);

    fn infer(&self, mut env: Env) -> Self::Out {
        match self {
            StatementKind::Let(let_stmt) => {
                let (bindings, pat) = let_stmt.name.infer(env.clone());

                let value = let_stmt.expr.infer(env.clone());

                unify::unify(env.clone(), value, pat);

                for (key, val) in bindings {
                    env.add_variable(key, val.into());
                }

                (env, Type::new(Mono::Unit))
            }
            StatementKind::Expr(expr_stmt) => (env.clone(), expr_stmt.infer(env)),
            StatementKind::Error => (env, Type::new(Mono::Error)),
        }
    }
}
