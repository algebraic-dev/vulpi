//! Inference of [Type]s

use vulpi_syntax::{
    r#abstract::TypeBinder,
    r#abstract::{self, TypeKind},
};

use crate::{
    env::Env,
    error::TypeErrorKind,
    kind::{Kind, KindType},
    types::Type,
};

use super::Infer;

impl Infer for r#abstract::Type {
    type Return = (Type, Kind);
    type Context = Env;

    fn infer(&self, context: &Self::Context) -> Self::Return {
        context.set_location(self.span.clone());

        match &self.data {
            TypeKind::Pi(pi) => {
                let (left, k) = pi.left.infer(context);
                k.unify(&Kind::star());

                let (right, k) = pi.right.infer(context);
                k.unify(&Kind::star());

                (Type::arrow(left, right), Kind::star())
            }
            TypeKind::Tuple(t) => {
                let types = t
                    .iter()
                    .map(|x| {
                        let (t, k) = x.infer(context);
                        k.unify(&Kind::star());
                        t
                    })
                    .collect();

                (Type::tuple(types), Kind::star())
            }
            TypeKind::Application(app) => {
                let (func, mut kind) = app.func.infer(context);

                let mut args = Vec::new();

                for arg in &app.args {
                    let (arg, k) = arg.infer(context);

                    match kind.as_ref() {
                        KindType::Arrow(l, r) => {
                            l.unify(&k);
                            kind = r.clone();
                        }
                        _ => {
                            context.report(TypeErrorKind::NotAFunctionKind);
                        }
                    }

                    args.push(arg);
                }

                (Type::app(func, args), kind)
            }
            TypeKind::Forall(forall) => {
                let mut ctx = context.clone();

                let mut binders = Vec::new();

                for binder in &forall.params {
                    let (param, kind) = match binder {
                        TypeBinder::Implicit(p) => (p, Kind::star()),
                        TypeBinder::Explicit(l, r) => (l, r.infer(&())),
                    };

                    ctx = context.add_ty(param.clone(), kind.clone());
                    binders.push((param.clone(), kind.clone()));
                }

                let (left, k) = forall.body.infer(&ctx);

                let forall = binders
                    .iter()
                    .fold(left, |acc, (x, k)| Type::forall(x.clone(), k.clone(), acc));

                (forall, k)
            }
            TypeKind::TypeVariable(t) => {
                let res = context.get_ty(t);

                match res {
                    Some(k) => (Type::named(t.clone()), k),
                    None => {
                        context.report(TypeErrorKind::UnboundTypeVariable(t.clone()));
                        (Type::error(), Kind::star())
                    }
                }
            }
            TypeKind::Type(q) => {
                let borrow_mut = &context.modules.borrow_mut();
                let module = borrow_mut.get(q.path).unwrap();
                let ty = module.types.get(&q.name).unwrap();
                (Type::variable(q.clone()), ty.clone())
            }
            TypeKind::Unit => {
                let k = Kind::star();
                (Type::tuple(vec![]), k)
            }
            TypeKind::Error => (Type::error(), Kind::error()),
        }
    }
}
