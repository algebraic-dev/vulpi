use im_rc::HashMap;
use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_syntax::{r#abstract::Pattern, r#abstract::PatternKind};

use crate::{apply::Apply, env::Env, types::Type, Infer};

impl Infer for Pattern {
    type Return = Type;

    type Context<'a> = (Env, &'a mut HashMap<Symbol, (Span, Type)>);

    fn infer(&self, (env, map): Self::Context<'_>) -> Self::Return {
        env.set_location(self.span.clone());

        match &self.data {
            PatternKind::Wildcard => env.new_hole(),
            PatternKind::Variable(name) => {
                let ty = env.new_hole();
                map.insert(name.clone(), (self.span.clone(), ty.clone()));
                ty
            }
            PatternKind::Literal(l) => l.infer(&env),
            PatternKind::Annotation(ann) => {
                let (ty, _) = ann.ty.infer(&env);
                let ty2 = ann.pat.infer((env.clone(), map));
                Type::unify(env, ty.clone(), ty2);
                ty
            }
            PatternKind::Or(or) => {
                let mut left_map = HashMap::new();
                let mut right_map = HashMap::new();

                let ty = or.left.infer((env.clone(), &mut left_map));
                let ty2 = or.right.infer((env.clone(), &mut right_map));

                Type::unify(env.clone(), ty.clone(), ty2);

                for (k, (span, left_ty)) in left_map {
                    let (_, right_ty) = right_map.get(&k).unwrap().clone();
                    env.set_location(span);
                    Type::unify(env.clone(), left_ty, right_ty);
                }

                ty
            }
            PatternKind::Application(app) => {
                let (mut ty, arity) = env.get_module_constructor(&app.func);

                if app.args.len() != arity {
                    env.report(crate::error::TypeErrorKind::WrongArity(
                        app.args.len(),
                        arity,
                    ));
                }

                for arg in &app.args {
                    ty = arg.apply(ty.clone(), (env.clone(), map))
                }

                ty
            }
            PatternKind::Error => Type::error(),
            PatternKind::Effect(_) => todo!(),
        }
    }
}
