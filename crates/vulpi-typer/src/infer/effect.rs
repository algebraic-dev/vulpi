//! Effect inference.

use vulpi_syntax::r#abstract::Effects;

use crate::{
    context::Context,
    r#type::{r#virtual::Env, real::Real, Type, TypeKind},
};

use super::Infer;

impl Infer for Option<Effects> {
    type Return = Type<Real>;
    type Context<'a> = (&'a mut Context, Env);

    fn infer(&self, (ctx, env): Self::Context<'_>) -> Self::Return {
        if let Some(effects) = self {
            let mut last = if let Some(rest) = &effects.rest {
                let (ty, kind) = rest.infer((ctx, env.clone()));
                ctx.subsumes(env.clone(), kind, crate::r#type::Kind::effect());
                ty
            } else {
                Type::new(TypeKind::Empty)
            };

            for effect in effects.effects.iter().rev() {
                env.on(effect.span.clone());

                let (ty, kind) = effect.infer((ctx, env.clone()));

                ctx.subsumes(env.clone(), kind, crate::r#type::Kind::effect());

                let (head, _) = ty.application_spine();

                let TypeKind::Variable(name) = head.as_ref() else { return ty };

                last = Type::new(TypeKind::Extend(name.clone(), ty, last));
            }

            last
        } else {
            Type::new(TypeKind::Empty)
        }
    }
}
