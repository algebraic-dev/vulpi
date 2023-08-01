use vulpi_syntax::r#abstract::Expr;

use crate::infer::Infer;
use crate::{
    env::Env,
    types::{HoleInner, Type, TypeKind},
};

use super::Apply;

impl Apply for Expr {
    type Return = Type;
    type Context<'a> = Env;

    fn apply(&self, ty: crate::types::Type, env: Self::Context<'_>) -> Self::Return {
        match ty.as_ref() {
            TypeKind::Hole(hole) => match hole.0.borrow().clone() {
                HoleInner::Filled(ty) => self.apply(ty, env),
                HoleInner::Empty(_) => {
                    let ret = env.new_hole();
                    let arg = self.infer(env);

                    hole.0
                        .replace(HoleInner::Filled(Type::arrow(arg, ret.clone())));

                    ret
                }
            },
            TypeKind::Arrow(l, r) => {
                let arg = self.infer(env.clone());
                Type::unify(env, arg, l.clone());
                r.clone()
            }
            TypeKind::Forall(p, _, t) => self.apply(t.instantiate(env.clone(), p.clone()), env),
            _ => {
                env.report(crate::error::TypeErrorKind::NotAFunction(
                    env.clone(),
                    ty.clone(),
                ));

                Type::error()
            }
        }
    }
}
