//! Inference of [Kind].

use vulpi_intern::Symbol;
use vulpi_syntax::{r#abstract::Kind, r#abstract::KindType};

use super::Infer;
use crate::kind;

impl Infer for Kind {
    type Return = kind::Kind;
    type Context<'a> = ();

    fn infer(&self, _ctx: Self::Context<'_>) -> Self::Return {
        match &self.data {
            KindType::Star => kind::Kind::star(),
            KindType::Arrow(l, r) => kind::Kind::arrow(l.infer(_ctx), r.infer(_ctx)),
            KindType::Effect => kind::Kind::var(Symbol::intern("Effect")),
            KindType::Constraint => kind::Kind::var(Symbol::intern("Constraint")),
            KindType::Error => kind::Kind::error(),
        }
    }
}
