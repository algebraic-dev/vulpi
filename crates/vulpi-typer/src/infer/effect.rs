use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract::Effects;

use crate::{
    env::Env,
    error::TypeErrorKind,
    kind::Kind,
    types::{self, TypeKind},
    Infer,
};

impl Infer for Option<Effects> {
    type Return = types::Type;

    type Context<'a> = &'a Env;

    fn infer(&self, context: Self::Context<'_>) -> Self::Return {
        let Some(effs) = self else {
            return types::Type::empty_row()
        };

        let mut row = types::Type::empty_row();
        let mut effects = Vec::new();

        for (i, eff) in effs.effects.iter().enumerate() {
            let is_last = i == effs.effects.len() - 1;

            let (ty, k) = eff.infer(context);

            k.unify(context, &Kind::var(Symbol::intern("Effect")));

            match ty.deref().as_ref() {
                TypeKind::Named(_) if is_last => row = ty,
                TypeKind::App(_, _) => match ty.destruct() {
                    Some((qual, _)) => effects.push((qual, ty.clone())),
                    _ => {
                        context.report(TypeErrorKind::NotEffect);
                        return types::Type::empty_row();
                    }
                },
                TypeKind::Variable(qual) => effects.push((qual.clone(), ty.clone())),
                _ => {
                    context.report(TypeErrorKind::NotEffect);
                    return types::Type::empty_row();
                }
            }
        }

        effects.into_iter().fold(row, |rest, (label, ty)| {
            types::Type::extend_row(label, ty, rest)
        })
    }
}
