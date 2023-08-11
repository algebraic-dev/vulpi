//! Inference of types

use crate::{
    context::Context,
    errors::TypeErrorKind,
    r#type::{
        self,
        eval::{Eval, Quote},
        r#virtual::Env,
        r#virtual::Virtual,
        real::{self, Real},
        Index, Kind, Type,
    },
};

use super::Infer;
use vulpi_syntax::{r#abstract, r#abstract::TypeKind};

impl Infer for r#abstract::Type {
    type Return = (Type<Real>, Kind<Virtual>);

    type Context<'a> = (&'a mut Context, Env);

    fn infer(&self, (ctx, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        match &self.data {
            TypeKind::Pi(pi) => {
                let (ty, kind) = pi.left.infer((ctx, env.clone()));
                env.on(pi.left.span.clone());
                ctx.subsumes(env.clone(), kind, Kind::typ());

                let (body, kind) = pi.right.infer((ctx, env.clone()));
                env.on(pi.right.span.clone());
                ctx.subsumes(env.clone(), kind, Kind::typ());

                let effs = pi.effects.infer((ctx, env.clone()));

                let typ = Type::new(r#type::TypeKind::Arrow(real::Pi { ty, effs, body }));
                (typ, Kind::typ())
            }
            TypeKind::Tuple(t) => {
                let mut types = Vec::new();

                for ty in t {
                    let (ty, kind) = ty.infer((ctx, env.clone()));
                    ctx.subsumes(env.clone(), kind, Kind::typ());
                    types.push(ty);
                }

                (Type::tuple(types), Kind::typ())
            }
            TypeKind::Application(app) => {
                let (ty, mut k) = app.func.infer((ctx, env.clone()));

                let mut args = Vec::new();

                for arg in &app.args {
                    env.on(arg.span.clone());

                    let (arg_ty, arg_kind) = arg.infer((ctx, env.clone()));

                    args.push(arg_ty);

                    if let Some((left, right)) = destruct_kind(ctx, env.clone(), k.deref()) {
                        ctx.subsumes(env.clone(), arg_kind, left);
                        k = right;
                    } else {
                        return (Type::error(), Kind::error());
                    }
                }

                (Type::application(ty, args), k)
            }
            TypeKind::Forall(forall) => {
                let mut env = env.clone();
                let mut names = Vec::new();

                for binder in &forall.params {
                    let (name, ty) = binder.infer((ctx, env.clone()));
                    env = env.add(Some(name.clone()), ty.clone());
                    names.push((name, ty));
                }

                let (ty, kind) = forall.body.infer((ctx, env.clone()));

                let forall = names.into_iter().fold(ty, |body, (name, kind)| {
                    let kind = kind.quote(env.level);
                    Type::forall(real::Forall { name, kind, body })
                });

                (forall, kind)
            }
            TypeKind::TypeVariable(name) => {
                let Some((index, kind)) = env.find(name) else {
                    ctx.report(&env, TypeErrorKind::CannotFind(name.clone()));
                    return (Type::error(), Type::error())
                };

                (Type::bound(Index(index)), kind)
            }
            TypeKind::Type(name) => (
                Type::variable(name.clone()),
                ctx.modules.typ(name).kind.eval(&env),
            ),
            TypeKind::Unit => (Type::tuple(Vec::new()), Kind::typ()),
            TypeKind::Error => (Type::error(), Kind::error()),
        }
    }
}

impl Infer for r#abstract::TypeBinder {
    type Return = (vulpi_intern::Symbol, Type<Virtual>);

    type Context<'a> = (&'a mut Context, Env);

    fn infer(&self, (ctx, env): Self::Context<'_>) -> Self::Return {
        match self {
            r#abstract::TypeBinder::Implicit(n) => (n.clone(), ctx.hole(&env, Kind::typ())),
            r#abstract::TypeBinder::Explicit(n, k) => (n.clone(), k.infer(env.clone()).eval(&env)),
        }
    }
}

fn destruct_kind(
    ctx: &mut Context,
    env: Env,
    k: Type<Virtual>,
) -> Option<(Type<Virtual>, Type<Virtual>)> {
    match k.deref().as_ref() {
        r#type::TypeKind::Arrow(arrow) => Some((arrow.ty.clone(), arrow.body.clone())),
        r#type::TypeKind::Error => None,
        _ => {
            let mes = TypeErrorKind::NotAFunction(env.clone(), k.quote(env.level));
            ctx.report(&env, mes);
            None
        }
    }
}
