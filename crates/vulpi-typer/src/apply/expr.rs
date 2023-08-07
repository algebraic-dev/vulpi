use vulpi_syntax::r#abstract::Expr;

use crate::ambient::Ambient;
use crate::check::Check;
use crate::infer::Infer;
use crate::{
    env::Env,
    types::{HoleInner, Type, TypeKind},
};

use super::Apply;

impl Apply for Expr {
    type Return = Type;
    type Context<'a> = (&'a mut Ambient, Env);

    fn apply(&self, ty: crate::types::Type, (ambient, env): Self::Context<'_>) -> Self::Return {
        match ty.as_ref() {
            TypeKind::Hole(hole) => match hole.0.borrow().clone() {
                HoleInner::Filled(ty) => self.apply(ty, (ambient, env)),
                HoleInner::Empty(_, k) => {
                    let ret = env.new_hole(k.clone());
                    let e = env.new_hole(k);
                    let arg = self.infer((ambient, env));

                    hole.0
                        .replace(HoleInner::Filled(Type::arrow(arg, e, ret.clone())));

                    ret
                }
                HoleInner::Lacks(_) => unreachable!(),
            },
            TypeKind::Arrow(l, ref effs, r) => {
                self.check(l.clone(), (ambient, env.clone()));

                let mut effs = effs.clone();

                println!("Causes {}", effs.show(env));

                while let TypeKind::RowExtend(name, typ, ty) = effs.deref().as_ref() {
                    effs = ty.clone();
                    ambient.causes(name.clone(), typ.clone())
                }
                r.clone()
            }
            TypeKind::Forall(p, k, t) => self.apply(
                t.instantiate(env.clone(), p.clone(), k.clone()),
                (ambient, env),
            ),
            TypeKind::Error => Type::error(),
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
