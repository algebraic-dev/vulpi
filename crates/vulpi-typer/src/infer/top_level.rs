use im_rc::HashMap;
use vulpi_syntax::r#abstract::LetDecl;

use crate::check::Check;
use crate::kind::Kind;
use crate::{env::Env, types::Type, Infer};

impl Infer for LetDecl {
    type Return = Type;

    type Context<'a> = Env;

    fn infer(&self, mut context: Self::Context<'_>) -> Self::Return {
        let mut all_tys = Vec::new();

        for binder in &self.binders {
            let mut bindings = HashMap::new();
            let pat_ty = binder.pattern.infer((context.clone(), &mut bindings));

            let fvs = binder.ty.data.free_variables();

            for f in fvs {
                // Rigid type variables.

                context.add_ty(f.clone(), Kind::star());
            }

            let (ty, _) = binder.ty.infer(&context);

            all_tys.push(ty.clone());

            Type::unify(context.clone(), pat_ty, ty);

            for binding in bindings {
                context.add_variable(binding.0, binding.1 .1)
            }
        }

        let size = self
            .body
            .cases
            .get(0)
            .map(|x| x.pattern.patterns.len())
            .unwrap_or(0);

        let collect = self
            .body
            .cases
            .iter()
            .map(|x| &x.pattern)
            .collect::<Vec<_>>();

        let ret = if let Some((_, res)) = &self.ret {
            res.infer(&context).0
        } else {
            context.new_hole()
        };

        (size, &collect).check(ret.clone(), context.clone());

        all_tys.into_iter().rfold(ret, |acc, x| Type::arrow(x, acc))
    }
}
