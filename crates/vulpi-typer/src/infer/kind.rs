use vulpi_syntax::r#abstract::{Kind, KindType};

use crate::{
    r#virtual::Env,
    real::{self, Real},
    Type, TypeKind,
};

use super::Infer;

impl Infer for Kind {
    type Return = crate::Kind<Real>;

    type Context<'a> = Env;

    fn infer(&self, context: Self::Context<'_>) -> Self::Return {
        context.on(self.span.clone());

        match &self.data {
            KindType::Star => Type::typ(),
            KindType::Constraint => todo!(),
            KindType::Arrow(l, r) => {
                let l = l.infer(context.clone());
                let r = r.infer(context);
                Type::new(TypeKind::Arrow(real::Arrow { typ: l, body: r }))
            }
            KindType::Error => Type::error(),
        }
    }
}
