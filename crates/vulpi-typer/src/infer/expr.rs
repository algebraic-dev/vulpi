//! Inference of expressions

use vulpi_syntax::r#abstract::{Expr, ExprKind};

use crate::{
    r#type::{r#virtual::Virtual, Effect},
    Context, Env, Type,
};

use super::Infer;

impl Infer for Expr {
    type Return = Type<Virtual>;

    type Context<'a> = (&'a mut Context, Env);

    fn infer(&self, (ctx, env): Self::Context<'_>) -> Self::Return {
        env.on(self.span.clone());

        match &self.data {
            ExprKind::Lambda(_) => todo!(),
            ExprKind::Application(_) => todo!(),
            ExprKind::Variable(_) => todo!(),
            ExprKind::Constructor(_) => todo!(),
            ExprKind::Function(_) => todo!(),
            ExprKind::Effect(_) => todo!(),
            ExprKind::Projection(_) => todo!(),
            ExprKind::Let(_) => todo!(),
            ExprKind::When(_) => todo!(),
            ExprKind::Do(_) => todo!(),
            ExprKind::Literal(n) => n.infer((ctx, env)),
            ExprKind::Annotation(_) => todo!(),
            ExprKind::RecordInstance(_) => todo!(),
            ExprKind::RecordUpdate(_) => todo!(),
            ExprKind::Handler(_) => todo!(),
            ExprKind::Cases(_) => todo!(),
            ExprKind::Tuple(_) => todo!(),
            ExprKind::Error => todo!(),
        }
    }
}
