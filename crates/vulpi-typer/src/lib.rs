//! This is the type checker for the Vulpi language. It is responsible for checking the types of
//! the abstract syntax tree and producing a better output with semantic information that is the
//! elaborated tree. The type checker of Vulpi is based on the bidirectional type checking with
//! higher rank polymorphism and higher kinded types.

use std::collections::HashSet;

use env::Env;
use module::Def;
use module::TypeData;
use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract::*;

pub mod ambient;
pub mod apply;
pub mod check;
pub mod coverage;
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

impl Declare for ExternalDecl {
    fn declare(&self, mut context: Env) {
        let fvs = self.ty.data.free_variables();

        for fv in fvs {
            context.types.insert(fv, kind::Kind::new_hole());
        }

        let (typ, k) = self.ty.infer(&context);
        k.unify(&context, &kind::Kind::star());

        context
            .modules
            .borrow_mut()
            .get(context.current_namespace())
            .variables
            .insert(self.name.clone(), typ);
    }
}

impl Declare for TypeDecl {
    fn declare(&self, mut context: Env) {
        let mut kinds = Vec::new();

        for binder in &self.binders {
            let (name, kind) = match binder {
                TypeBinder::Implicit(p) => (p.clone(), kind::Kind::star()),
                TypeBinder::Explicit(p, k) => (p.clone(), k.infer(())),
            };

            context.types.insert(name, kind.clone());
            kinds.push(kind);
        }

        let arrow = kinds
            .into_iter()
            .rfold(kind::Kind::star(), |acc, x| kind::Kind::arrow(x, acc));

        let def = match &self.def {
            TypeDef::Sum(cons) => {
                Def::Enum(cons.constructors.iter().map(|x| x.name.clone()).collect())
            }
            TypeDef::Record(rec) => Def::Record(rec.fields.iter().map(|x| x.0.clone()).collect()),
            TypeDef::Synonym(_) => Def::Type,
            TypeDef::Abstract => Def::Type,
        };

        context
            .modules
            .borrow_mut()
            .get(self.name.path.clone())
            .types
            .insert(
                self.name.name.clone(),
                TypeData {
                    kind: arrow,
                    module: self.namespace.clone(),
                    binders: self.binders.len(),
                    def,
                },
            );
    }

    fn define(&self, mut context: Env) {
        let mut kinds = Vec::new();
        let mut types = Vec::new();

        for binder in &self.binders {
            let (name, kind) = match binder {
                TypeBinder::Implicit(p) => (p.clone(), kind::Kind::star()),
                TypeBinder::Explicit(p, k) => (p.clone(), k.infer(())),
            };

            context.types.insert(name.clone(), kind.clone());

            kinds.push((name.clone(), kind));
            types.push(types::Type::named(name));
        }

        let ret = types::Type::app(types::Type::variable(self.name.clone()), types);

        match &self.def {
            TypeDef::Sum(sum) => {
                for cons in &sum.constructors {
                    let types = cons.args.iter().map(|x| {
                        let (t, k) = x.infer(&context);
                        k.unify(&context, &kind::Kind::star());
                        t
                    });

                    let ret = if let Some(new_ret) = &cons.typ {
                        let (t, k) = new_ret.infer(&context);
                        k.unify(&context, &kind::Kind::star());

                        let foralled = kinds
                            .clone()
                            .into_iter()
                            .rfold(ret.clone(), |acc, (n, k)| types::Type::forall(n, k, acc));

                        types::Type::sub(&foralled, context.clone(), t.clone());

                        t
                    } else {
                        ret.clone()
                    };

                    let typ = types.rfold(ret.clone(), |acc, x| {
                        types::Type::arrow(x, types::Type::empty_row(), acc)
                    });

                    let typ = kinds
                        .clone()
                        .into_iter()
                        .rfold(typ.clone(), |acc, (n, k)| types::Type::forall(n, k, acc));

                    context
                        .modules
                        .borrow_mut()
                        .get(cons.name.path.clone())
                        .constructors
                        .insert(cons.name.name.clone(), (typ, cons.args.len()));
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

                    context
                        .modules
                        .borrow_mut()
                        .get(field.0.path.clone())
                        .fields
                        .insert(field.0.name.clone(), typ);
                }
            }
            TypeDef::Synonym(_) => todo!(),
            TypeDef::Abstract => (),
        }
    }
}

impl Declare for EffectDecl {
    fn declare(&self, mut context: Env) {
        let mut kinds = Vec::new();

        for binder in &self.binders {
            let (name, kind) = match binder {
                TypeBinder::Implicit(p) => (p.clone(), kind::Kind::star()),
                TypeBinder::Explicit(p, k) => (p.clone(), k.infer(())),
            };

            context.types.insert(name, kind.clone());
            kinds.push(kind);
        }

        let arrow = kinds
            .into_iter()
            .rfold(kind::Kind::var(Symbol::intern("Effect")), |acc, x| {
                kind::Kind::arrow(x, acc)
            });

        context
            .modules
            .borrow_mut()
            .get(self.qualified.path.clone())
            .types
            .insert(
                self.qualified.name.clone(),
                TypeData {
                    kind: arrow,
                    module: self.namespace.clone(),
                    binders: self.binders.len(),
                    def: Def::Effect(self.fields.iter().map(|x| x.name.clone()).collect()),
                },
            );
    }

    fn define(&self, mut context: Env) {
        let mut kinds = Vec::new();
        let mut binders = Vec::new();

        for binder in &self.binders {
            let (name, kind) = match binder {
                TypeBinder::Implicit(p) => (p.clone(), kind::Kind::star()),
                TypeBinder::Explicit(p, k) => (p.clone(), k.infer(())),
            };

            context.types.insert(name.clone(), kind.clone());

            kinds.push((name.clone(), kind));
            binders.push(types::Type::named(name));
        }

        let effects = types::Type::app(
            types::Type::variable(self.qualified.clone()),
            binders.clone(),
        );

        for eff in &self.fields {
            let mut types = eff
                .args
                .iter()
                .map(|x| {
                    let (t, k) = x.infer(&context);
                    k.unify(&context, &kind::Kind::star());
                    t
                })
                .collect::<Vec<_>>();

            let (init, k) = eff.ty.infer(&context);
            k.unify(&context, &kind::Kind::star());

            let first = types.pop().unwrap_or_else(|| {
                types::Type::app(
                    types::Type::variable(context.import("Unit").unwrap()),
                    vec![],
                )
            });

            let init = types::Type::arrow(
                first,
                types::Type::extend_row(
                    self.qualified.clone(),
                    effects.clone(),
                    types::Type::empty_row(),
                ),
                init.clone(),
            );

            let typ = types.into_iter().rfold(init, |acc, x| {
                types::Type::arrow(x, types::Type::empty_row(), acc)
            });

            let typ = kinds
                .clone()
                .into_iter()
                .rfold(typ.clone(), |acc, (n, k)| types::Type::forall(n, k, acc));

            context
                .modules
                .borrow_mut()
                .get(eff.name.path.clone())
                .effects
                .insert(eff.name.name.clone(), typ);
        }
    }
}

impl Declare for LetDecl {
    fn declare(&self, mut context: Env) {
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
            context.types.insert(fv.clone(), kind::Kind::new_hole());
        }

        let (e, ret) = match &self.ret {
            Some((e, t)) => {
                let (t, k) = t.infer(&context);
                k.unify(&context, &kind::Kind::star());
                (e.infer(&context), t)
            }
            None => (
                types::Type::empty_row(),
                context.new_hole(kind::Kind::star()),
            ),
        };

        let mut types = self
            .binders
            .iter()
            .map(|x| {
                let (t, k) = x.ty.infer(&context);
                k.unify(&context, &kind::Kind::star());
                t
            })
            .collect::<Vec<_>>();

        let first = types.pop().unwrap_or_else(|| {
            types::Type::app(
                types::Type::variable(context.import("Unit").unwrap()),
                vec![],
            )
        });

        let ret = types::Type::arrow(first, e, ret);

        let typ = types.into_iter().rfold(ret, |acc, x| {
            types::Type::arrow(x, types::Type::empty_row(), acc)
        });

        let typ = fvs.into_iter().fold(typ, |acc, x| {
            types::Type::forall(x, kind::Kind::star(), acc)
        });

        context
            .modules
            .borrow_mut()
            .get(context.current_namespace())
            .variables
            .insert(self.name.clone(), typ);
    }

    fn define(&self, context: Env) {
        self.infer(context);
    }
}

impl Declare for ModuleDecl {
    fn declare(&self, mut context: Env) {
        context.on(self.namespace.clone(), |context| {
            if let Some(types) = self.types() {
                for decl in types {
                    decl.declare(context.clone());
                }
            }

            if let Some(effs) = self.effects() {
                for decl in effs {
                    decl.declare(context.clone());
                }
            }

            if let Some(modules) = self.modules() {
                for decl in modules {
                    decl.declare(context.clone());
                }
            }

            if let Some(lets) = self.lets() {
                for decl in lets {
                    decl.declare(context.clone());
                }
            }

            if let Some(externals) = self.externals() {
                for decl in externals {
                    decl.declare(context.clone());
                }
            }
        })
    }

    fn define(&self, mut context: Env) {
        context.on(self.namespace.clone(), |context| {
            if let Some(types) = self.types() {
                for decl in types {
                    decl.define(context.clone());
                }
            }

            if let Some(effs) = self.effects() {
                for decl in effs {
                    decl.define(context.clone());
                }
            }

            if let Some(modules) = self.modules() {
                for decl in modules {
                    decl.define(context.clone());
                }
            }

            if let Some(lets) = self.lets() {
                for decl in lets {
                    decl.define(context.clone());
                }
            }

            if let Some(externals) = self.externals() {
                for decl in externals {
                    decl.define(context.clone());
                }
            }
        })
    }
}

impl Declare for Module {
    fn declare(&self, context: Env) {
        for decl in self.types() {
            decl.declare(context.clone());
        }

        for effs in self.effects() {
            effs.declare(context.clone());
        }

        for modules in self.modules() {
            modules.declare(context.clone());
        }

        for del in self.lets() {
            del.declare(context.clone());
        }

        for del in self.externals() {
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

        for effs in self.effects() {
            effs.define(context.clone());
        }

        for lets in self.lets() {
            lets.define(context.clone());
        }

        for del in self.externals() {
            del.define(context.clone());
        }
    }
}
