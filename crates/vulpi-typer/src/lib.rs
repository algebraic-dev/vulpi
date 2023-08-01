//! This is the type checker for the Vulpi language. It is responsible for checking the types of
//! the abstract syntax tree and producing a better output with semantic information that is the
//! elaborated tree. The type checker of Vulpi is based on the bidirectional type checking with
//! higher rank polymorphism and higher kinded types.

use std::collections::HashSet;

use env::Env;
use vulpi_syntax::r#abstract::*;

pub mod env;
pub mod error;
pub mod infer;
pub mod kind;
pub mod module;
pub mod types;

pub use infer::types::*;
pub use infer::Infer;

pub trait Declare {
    fn declare(&self, context: Env);
    fn define(&self, _context: Env) {}
}

impl Declare for TypeDecl {
    fn declare(&self, mut context: Env) {
        let mut kinds = Vec::new();

        for binder in &self.binders {
            let (name, kind) = match binder {
                TypeBinder::Implicit(p) => (p.clone(), kind::Kind::star()),
                TypeBinder::Explicit(p, k) => (p.clone(), k.infer(&())),
            };

            context.types.insert(name, kind.clone());
            kinds.push(kind);
        }

        let arrow = kinds
            .into_iter()
            .rfold(kind::Kind::star(), |acc, x| kind::Kind::arrow(x, acc));

        context.modules.borrow_mut().modules[self.id]
            .types
            .insert(self.name.clone(), arrow);
    }

    fn define(&self, mut context: Env) {
        let mut kinds = Vec::new();
        let mut types = Vec::new();

        for binder in &self.binders {
            let (name, kind) = match binder {
                TypeBinder::Implicit(p) => (p.clone(), kind::Kind::star()),
                TypeBinder::Explicit(p, k) => (p.clone(), k.infer(&())),
            };

            context.types.insert(name.clone(), kind.clone());

            kinds.push((name.clone(), kind));
            types.push(types::Type::named(name));
        }

        let ret = types::Type::app(types::Type::named(self.name.clone()), types);

        match &self.def {
            TypeDef::Sum(sum) => {
                for cons in &sum.constructors {
                    let types = cons.args.iter().map(|x| {
                        let (t, k) = x.infer(&context);
                        k.unify(&context, &kind::Kind::star());
                        t
                    });

                    let typ = types.rfold(ret.clone(), |acc, x| types::Type::arrow(x, acc));

                    let typ = kinds
                        .clone()
                        .into_iter()
                        .rfold(typ.clone(), |acc, (n, k)| types::Type::forall(n, k, acc));

                    println!("{}", typ.show(context.clone()));

                    context.modules.borrow_mut().modules[context.current_id()]
                        .constructors
                        .insert(cons.name.clone(), typ);
                }
            }
            TypeDef::Record(rec) => {
                for field in &rec.fields {
                    let (t, k) = field.1.infer(&context);

                    let typ = kinds
                        .clone()
                        .into_iter()
                        .rfold(t.clone(), |acc, (n, k)| types::Type::forall(n, k, acc));

                    k.unify(&context, &kind::Kind::star());

                    context.modules.borrow_mut().modules[context.current_id()]
                        .fields
                        .insert(field.0.clone(), typ);
                }
            }
            TypeDef::Synonym(_) => todo!(),
            TypeDef::Abstract => (),
        }
    }
}

impl Declare for LetDecl {
    fn declare(&self, mut context: Env) {
        let fvs = self
            .binders
            .iter()
            .map(|x| x.ty.data.free_variables())
            .fold(HashSet::new(), |acc, x| acc.union(&x).cloned().collect());

        for fv in &fvs {
            context.types.insert(fv.clone(), kind::Kind::star());
        }

        let ret = match &self.ret {
            Some((_, t)) => {
                let (t, k) = t.infer(&context);
                k.unify(&context, &kind::Kind::star());
                t
            }
            None => context.new_hole(),
        };

        let typ = self
            .binders
            .iter()
            .map(|x| {
                let (t, k) = x.ty.infer(&context);
                k.unify(&context, &kind::Kind::star());
                t
            })
            .rfold(ret, |acc, x| types::Type::arrow(x, acc));

        let typ = fvs.into_iter().fold(typ, |acc, x| {
            types::Type::forall(x, kind::Kind::star(), acc)
        });

        context.modules.borrow_mut().modules[context.current_id()]
            .variables
            .insert(self.name.clone(), typ);
    }
}

impl Declare for ModuleDecl {
    fn declare(&self, mut context: Env) {
        context.on(self.id, |context| {
            if let Some(types) = self.types() {
                for decl in types {
                    decl.declare(context.clone());
                }
            }

            if let Some(lets) = self.lets() {
                for decl in lets {
                    decl.declare(context.clone());
                }
            }

            if let Some(modules) = self.modules() {
                for decl in modules {
                    decl.declare(context.clone());
                }
            }
        })
    }

    fn define(&self, mut context: Env) {
        context.on(self.id, |context| {
            if let Some(types) = self.types() {
                for decl in types {
                    decl.define(context.clone());
                }
            }

            if let Some(lets) = self.lets() {
                for decl in lets {
                    decl.define(context.clone());
                }
            }

            if let Some(modules) = self.modules() {
                for decl in modules {
                    decl.define(context.clone());
                }
            }
        })
    }
}

impl Declare for Module {
    fn declare(&self, context: Env) {
        for modules in self.modules() {
            modules.declare(context.clone());
        }

        for decl in self.types() {
            decl.declare(context.clone());
        }

        for del in self.lets() {
            del.declare(context.clone());
        }
    }

    fn define(&self, context: Env) {
        for module in self.modules() {
            module.define(context.clone());
        }

        for decl in self.types() {
            decl.define(context.clone());
        }
    }
}
