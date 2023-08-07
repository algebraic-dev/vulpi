use std::collections::HashSet;

use im_rc::HashMap;
use vulpi_syntax::r#abstract::LetDecl;

use crate::ambient::Ambient;
use crate::check::Check;
use crate::kind::Kind;
use crate::{env::Env, types::Type, Infer};

impl Infer for LetDecl {
    type Return = Type;

    type Context<'a> = Env;

    fn infer(&self, mut env: Self::Context<'_>) -> Self::Return {
        let fvs = self
            .binders
            .iter()
            .map(|x| x.ty.data.free_variables())
            .chain(std::iter::once(
                self.ret
                    .as_ref()
                    .map(|x| x.1.data.free_variables())
                    .unwrap_or_default(),
            ))
            .fold(HashSet::new(), |acc, x| acc.union(&x).cloned().collect());

        for fv in &fvs {
            env.types.insert(fv.clone(), Kind::new_hole());
        }

        let mut all_tys = Vec::new();

        for binder in &self.binders {
            let mut bindings = HashMap::new();
            let pat_ty = binder.pattern.infer((env.clone(), &mut bindings));

            let (ty, _) = binder.ty.infer(&env);

            all_tys.push(ty.clone());

            Type::unify(env.clone(), pat_ty, ty);

            for binding in bindings {
                env.add_variable(binding.0, binding.1 .1)
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

        let (effs, ret) = if let Some((effs, res)) = &self.ret {
            (effs.infer(&env), res.infer(&env).0)
        } else {
            (Type::empty_row(), env.new_hole(Kind::star()))
        };

        let mut ambient = Ambient::default();
        (size, &collect).check(ret.clone(), (&mut ambient, env.clone()));

        println!(
            "{:?}",
            ambient
                .effects
                .keys()
                .map(|x| format!("{}.{}", x.name.get(), x.path.get()))
                .collect::<Vec<_>>()
        );

        all_tys
            .into_iter()
            .rfold(ret, |acc, x| Type::arrow(x, Type::empty_row(), acc))
    }
}
