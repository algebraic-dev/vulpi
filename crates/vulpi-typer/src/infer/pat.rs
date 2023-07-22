//! Inference of patterns

use std::collections::HashMap;

use vulpi_storage::interner::Symbol;
use vulpi_syntax::resolved::{PatAnnotation, PatApplication, PatOr, PatternKind};

use crate::{
    types::{Kind, KindType, Mono, Type},
    unify::{unify, unify_kinds},
};

use super::Infer;

impl Infer for PatAnnotation {
    type Out = (HashMap<Symbol, Type>, Type);

    fn infer(&self, env: crate::context::Env) -> Self::Out {
        let (map, typ) = self.pat.infer(env.clone());
        let (kind, ty) = self.ty.infer(env.clone());

        unify_kinds(env.clone(), kind, Kind::new(KindType::Star));
        unify(env, typ, ty.clone());

        (map, ty)
    }
}

impl Infer for PatOr {
    type Out = (HashMap<Symbol, Type>, Type);

    fn infer(&self, env: crate::context::Env) -> Self::Out {
        let (left_map, left) = self.left.infer(env.clone());
        let (right_map, right) = self.right.infer(env.clone());

        // Here we assume that left_map and right_map contains the same keys because of the
        // resolution process.
        for (key, val) in left_map {
            let right_val = right_map.get(&key).unwrap();
            unify(env.clone(), val, right_val.clone());
        }

        unify(env, left.clone(), right);

        (right_map, left)
    }
}

impl Infer for PatApplication {
    type Out = (HashMap<Symbol, Type>, Type);

    fn infer(&self, mut env: crate::context::Env) -> Self::Out {
        let scheme = env
            .modules
            .borrow_mut()
            .get_cons(self.func.canonical, &self.func.last)
            .unwrap()
            .clone();

        if self.args.len() != scheme.arity {
            env.report(crate::error::TypeErrorKind::WrongArity(
                self.func.last.clone(),
                scheme.arity,
                self.args.len(),
            ));
            return (HashMap::new(), Type::new(Mono::Error));
        }

        let mut typ = env.instantiate(scheme.typ);
        let mut args = self.args.as_slice();
        let mut bindings = HashMap::new();

        while let Mono::Function(left, right) = &*typ {
            let (left_bindings, left_pat_ty) = args[0].infer(env.clone());
            unify(env.clone(), left_pat_ty, left.clone());
            args = &args[1..];
            bindings.extend(left_bindings);
            typ = right.clone();
        }

        (bindings, typ)
    }
}

impl Infer for PatternKind {
    type Out = (HashMap<Symbol, Type>, Type);

    fn infer(&self, mut env: crate::context::Env) -> Self::Out {
        match self {
            PatternKind::Wildcard => {
                let typ = env.new_hole();
                (HashMap::new(), typ)
            }
            PatternKind::Lower(l) => {
                let typ = env.new_hole();
                let mut map = HashMap::new();
                map.insert(l.data.clone(), typ.clone());
                (map, typ)
            }
            PatternKind::Literal(l) => {
                let typ = l.infer(env);
                (HashMap::new(), typ)
            }
            PatternKind::Annotation(ann) => ann.infer(env),
            PatternKind::Or(or) => or.infer(env),
            PatternKind::Application(app) => app.infer(env),
            PatternKind::Error => (HashMap::new(), Type::new(Mono::Error)),
        }
    }
}
