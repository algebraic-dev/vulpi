//! Inference of [Kind].

use vulpi_syntax::{r#abstract::Kind, r#abstract::KindType};

use super::Infer;
use crate::kind;

impl Infer for Kind {
    type Return = kind::Kind;
    type Context = ();

    fn infer(&self, _ctx: &Self::Context) -> Self::Return {
        match &self.data {
            KindType::Star => kind::Kind::star(),
            KindType::Arrow(l, r) => kind::Kind::arrow(l.infer(_ctx), r.infer(_ctx)),
        }
    }
}
