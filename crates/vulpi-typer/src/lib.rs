//! This is the entrypoint for the `vulpi-typer` crate. It is responsible for type checking a
//! higher rank, higher kinded, algebraic type system. It is also responsible for type inference
//! and type checking of the ambient effects system.

pub use context::Context;
use coverage::{Problem, Witness};
use errors::TypeErrorKind;
use r#type::real::Arrow;
use r#type::TypeKind;
pub use r#type::{r#virtual::Env, Type};
pub use r#type::{r#virtual::Virtual, real::Real, Kind};
use vulpi_report::Report;
use vulpi_syntax::r#abstract::LetDecl;

use crate::check::Check;
use crate::r#type::eval::{Eval, Quote};
use crate::r#type::real::Forall;
use crate::r#type::Index;
use infer::Infer;
use module::{Def, LetDef, TypeData};

use vulpi_syntax::{
    elaborated,
    r#abstract::{EffectDecl, ExternalDecl, ModuleDecl, Program, TypeDecl, TypeDef},
};

pub mod check;
pub mod context;
pub mod coverage;
pub mod errors;
pub mod infer;
pub mod module;
pub mod r#type;

/// Trait for declaration of top level items inside the type checker.
pub trait Declare {
    fn declare(&self, context: (&mut Context, Env));
    fn define(&self, _context: (&mut Context, Env)) {}
}

impl<T: Declare> Declare for Vec<T> {
    fn declare(&self, context: (&mut Context, Env)) {
        for decl in self {
            decl.declare((context.0, context.1.clone()));
        }
    }

    fn define(&self, context: (&mut Context, Env)) {
        for decl in self {
            decl.define((context.0, context.1.clone()));
        }
    }
}

impl<T: Declare> Declare for Option<T> {
    fn declare(&self, context: (&mut Context, Env)) {
        if let Some(decl) = self {
            decl.declare(context);
        }
    }

    fn define(&self, context: (&mut Context, Env)) {
        if let Some(decl) = self {
            decl.define(context);
        }
    }
}

impl Declare for TypeDecl {
    fn declare(&self, (context, env): (&mut Context, Env)) {
        let vec = &self.binders;

        let mut names = Vec::new();
        let mut binders = Vec::new();

        for binder in vec {
            let (n, binder) = binder.infer((context, env.clone()));
            binders.push(binder.eval(&env));
            names.push(n);
        }

        let kind = Type::<Virtual>::function(binders.clone(), Type::typ());

        let type_def = &self.def;
        let def = get_definition_of_type(type_def);

        context.modules.get(&self.name.path).types.insert(
            self.name.name.clone(),
            TypeData {
                kind,
                binders: names.into_iter().zip(binders.into_iter()).collect(),
                module: self.namespace.clone(),
                def,
            },
        );
    }

    fn define(&self, (ctx, mut env): (&mut Context, Env)) {
        let type_decl = ctx.modules.typ(&self.name);

        for (name, binder) in &type_decl.binders {
            env = env.add(Some(name.clone()), binder.clone());
        }

        let ret_type = Type::<Real>::application(
            Type::variable(self.name.clone()),
            (0..type_decl.binders.len())
                .rev()
                .map(|x| Type::bound(Index(x)))
                .collect(),
        );

        let decl = match &self.def {
            TypeDef::Sum(cons) => {
                let mut constructors = Vec::new();

                let mut cons_types = Vec::new();

                for cons in &cons.constructors {
                    constructors.push((cons.name.clone(), cons.args.len()));

                    let mut types = Vec::new();

                    for arg in &cons.args {
                        env.on(arg.span.clone());
                        let (typ, kind) = arg.infer((ctx, env.clone()));
                        ctx.subsumes(env.clone(), kind, Kind::typ());
                        types.push(typ);
                    }

                    let typ = Type::<Real>::function(types, ret_type.clone());
                    cons_types.push((cons.name.clone(), cons.args.len(), typ));
                }

                for (name, arity, mut cons_typ) in cons_types {
                    for (name, binder) in type_decl.binders.iter().rev() {
                        cons_typ = Type::forall(Forall {
                            name: name.clone(),
                            kind: binder.clone().quote(env.level),
                            body: cons_typ,
                        });
                    }

                    ctx.modules
                        .get(&name.path)
                        .constructors
                        .insert(name.name.clone(), (cons_typ, arity, self.name.clone()));
                }

                elaborated::TypeDecl::Enum(constructors)
            }
            TypeDef::Record(rec) => {
                let mut types = Vec::new();
                let mut names = Vec::new();

                for field in &rec.fields {
                    names.push(field.0.clone());

                    let (typ, kind) = field.1.infer((ctx, env.clone()));
                    env.on(field.1.span.clone());

                    ctx.subsumes(env.clone(), kind, Kind::typ());

                    types.push(typ);
                }

                for (name, mut typ) in names.iter().zip(types.into_iter()) {
                    for (name, binder) in type_decl.binders.iter().rev() {
                        typ = Type::forall(Forall {
                            name: name.clone(),
                            kind: binder.clone().quote(env.level),
                            body: typ,
                        });
                    }

                    ctx.modules
                        .get(&name.path)
                        .fields
                        .insert(name.name.clone(), typ);
                }

                elaborated::TypeDecl::Record(names)
            }
            TypeDef::Synonym(_) => todo!(),
            TypeDef::Abstract => elaborated::TypeDecl::Abstract,
        };

        ctx.elaborated.types.insert(self.name.clone(), decl);
    }
}

fn get_definition_of_type(type_def: &TypeDef) -> Def {
    match type_def {
        TypeDef::Sum(cons) => Def::Enum(cons.constructors.iter().map(|x| x.name.clone()).collect()),
        TypeDef::Record(rec) => Def::Record(rec.fields.iter().map(|x| x.0.clone()).collect()),
        TypeDef::Synonym(_) => Def::Type,
        TypeDef::Abstract => Def::Type,
    }
}

impl Declare for ExternalDecl {
    fn declare(&self, (ctx, mut env): (&mut Context, Env)) {
        let fvs = self.ty.data.free_variables();

        let mut unbound = Vec::new();

        for fv in fvs {
            let ty = ctx.hole(&env, Type::typ());
            env = env.add(Some(fv.clone()), ty.clone());
            unbound.push((fv, ty.quote(env.level)))
        }

        let (typ, k) = self.ty.infer((ctx, env.clone()));
        ctx.subsumes(env.clone(), k, Kind::typ());

        let typ = typ.eval(&env);

        ctx.modules.get(&self.namespace).variables.insert(
            self.name.name.clone(),
            LetDef {
                typ: typ.clone(),
                unbound,
                ambient: Type::new(TypeKind::Empty),
                unbound_effects: vec![],
                ret: typ.clone(),
                args: vec![],
            },
        );

        ctx.elaborated.externals.insert(
            self.name.clone(),
            elaborated::ExternalDecl {
                typ: typ.quote(env.level),
                binding: self.ret.clone(),
            },
        );
    }
}

impl Declare for EffectDecl {
    fn declare(&self, (context, env): (&mut Context, Env)) {
        let mut binders = Vec::new();
        let mut names = Vec::new();

        for binder in &self.binders {
            let (n, binder) = binder.infer((context, env.clone()));
            binders.push(binder.eval(&env));
            names.push(n);
        }

        let kind = Type::<Virtual>::function(binders.clone(), Type::effect());

        let effects = self
            .effects
            .iter()
            .map(|x| x.name.clone())
            .collect::<Vec<_>>();

        context.modules.get(&self.name.path.clone()).types.insert(
            self.name.name.clone(),
            TypeData {
                kind,
                binders: names.into_iter().zip(binders.into_iter()).collect(),
                module: self.namespace.clone(),
                def: Def::Effect(effects.clone()),
            },
        );

        context
            .elaborated
            .effects
            .insert(self.name.clone(), effects);
    }

    fn define(&self, (ctx, mut env): (&mut Context, Env)) {
        let type_decl = ctx.modules.typ(&self.name);

        for (name, binder) in &type_decl.binders {
            env = env.add(Some(name.clone()), binder.clone());
        }

        let mut names = Vec::new();
        let mut eff_types = Vec::new();

        for eff in &self.effects {
            let mut env = env.clone();
            names.push(eff.name.clone());

            let mut fvs = eff.ty.data.free_variables();
            let mut bound = Vec::new();

            for arg in &eff.args {
                fvs.extend(arg.data.free_variables());
            }

            for (name, _) in &type_decl.binders {
                fvs.remove(name);
            }

            let size = fvs.len();

            for fv in fvs {
                let ty = ctx.hole(&env, Type::typ());
                env = env.add(Some(fv.clone()), ty.clone());
                bound.push((fv, ty));
            }

            let extension_type_name = ctx.new_name();
            let hole = ctx.hole(&env, Type::typ());
            env = env.add(Some(extension_type_name.clone()), hole);

            let eff_type = Type::<Real>::application(
                Type::variable(self.name.clone()),
                (0..type_decl.binders.len())
                    .rev()
                    .map(|x| Type::bound(Index(x + size + 1)))
                    .collect(),
            );

            let mut types = Vec::new();

            for arg in &eff.args {
                env.on(arg.span.clone());
                let (typ, kind) = arg.infer((ctx, env.clone()));
                ctx.subsumes(env.clone(), kind, Kind::typ());
                types.push(typ);
            }

            if eff.args.is_empty() {
                ctx.report(&env, errors::TypeErrorKind::AtLeastOneArgument)
            } else {
                let (ret_type, kind) = eff.ty.infer((ctx, env.clone()));
                env.on(eff.ty.span.clone());
                ctx.subsumes(env.clone(), kind, Kind::typ());

                let popped = types.pop().unwrap();

                let extension_type = Type::<Real>::bound(Index(0));

                let fun = Type::new(TypeKind::<Real>::Arrow(Arrow {
                    ty: popped,
                    effs: Type::<Real>::extend(
                        self.name.clone(),
                        eff_type.clone(),
                        extension_type.clone(),
                    ),
                    body: ret_type,
                }));

                let mut typ = Type::<Real>::function(types, fun.clone());

                typ = Type::forall(Forall {
                    name: extension_type_name.clone(),
                    kind: Type::row(),
                    body: typ,
                });

                for (name, kind) in bound {
                    typ = Type::forall(Forall {
                        name,
                        kind: kind.quote(env.level),
                        body: typ,
                    });
                }

                eff_types.push((eff.args.len(), typ));
            }
        }

        for (name, (arity, mut cons_typ)) in names.iter().zip(eff_types.into_iter()) {
            for (name, binder) in type_decl.binders.iter().rev() {
                cons_typ = Type::forall(Forall {
                    name: name.clone(),
                    kind: binder.clone().quote(env.level),
                    body: cons_typ,
                });
            }

            ctx.modules.get(&name.path).effects.insert(
                name.name.clone(),
                (cons_typ.eval(&env), self.name.clone(), arity),
            );
        }
    }
}

impl Declare for LetDecl {
    fn declare(&self, (ctx, mut env): (&mut Context, Env)) {
        let has_effect = self.ret.as_ref().map(|x| x.0.is_some()).unwrap_or_default();

        if has_effect && self.binders.is_empty() {
            ctx.report(&env, errors::TypeErrorKind::AtLeastOneArgument);
            return;
        }

        let mut efvs = self
            .ret
            .as_ref()
            .and_then(|x| {
                x.0.as_ref()
                    .and_then(|x| x.rest.as_ref().map(|x| x.data.free_effects()))
            })
            .unwrap_or_default();

        let mut fvs = self
            .ret
            .as_ref()
            .map(|x| x.1.data.free_variables())
            .unwrap_or_default();

        for arg in &self.binders {
            fvs.extend(arg.ty.data.free_variables());
            efvs.extend(arg.ty.data.free_effects());
        }

        let mut unbound = Vec::new();
        let mut effect_bounds = Vec::new();

        for fv in fvs {
            let ty = ctx.hole(&env, Type::typ());
            env = env.add(Some(fv.clone()), ty.clone());
            unbound.push((fv, ty.quote(env.level)));
        }

        for fv in efvs {
            let ty = ctx.lacks(&env, Default::default());
            env = env.add(Some(fv.clone()), ty.clone());
            effect_bounds.push((fv, ty.quote(env.level)));
        }

        let mut args = Vec::new();

        for arg in &self.binders {
            let (ty, kind) = arg.ty.infer((ctx, env.clone()));
            env.on(arg.ty.span.clone());

            ctx.subsumes(env.clone(), kind, Kind::typ());

            args.push(ty);
        }

        let (effs, ret) = if let Some((eff, ret)) = &self.ret {
            let effs = eff.infer((ctx, env.clone()));

            let (ty, kind) = ret.infer((ctx, env.clone()));
            env.on(ret.span.clone());
            ctx.subsumes(env.clone(), kind, Kind::typ());

            (effs, ty)
        } else {
            (Type::new(TypeKind::Empty), ctx.hole(&env, Kind::typ()))
        };

        let func_args = args.clone();

        let ret_type = if has_effect {
            let ty = args.pop().unwrap();
            Type::new(TypeKind::Arrow(Arrow {
                ty,
                effs: effs.clone(),
                body: ret.clone(),
            }))
        } else {
            ret.clone()
        };

        let mut typ = Type::<Real>::function(args.clone(), ret_type);

        for (name, _) in effect_bounds.clone() {
            typ = Type::forall(Forall {
                name,
                kind: Type::row(),
                body: typ,
            });
        }

        for (name, kind) in unbound.clone() {
            typ = Type::forall(Forall {
                name,
                kind,
                body: typ,
            });
        }

        ctx.modules.get(&self.name.path.clone()).variables.insert(
            self.name.name.clone(),
            LetDef {
                typ: typ.eval(&env),
                unbound,
                ambient: effs,
                unbound_effects: effect_bounds,
                ret: ret.eval(&env),
                args: func_args,
            },
        );
    }

    fn define(&self, (ctx, mut env): (&mut Context, Env)) {
        env.on(self.span.clone());

        let let_decl = ctx.modules.let_decl(&self.name).clone();

        for (fv, ty) in &let_decl.unbound {
            env = env.add(Some(fv.clone()), ty.eval(&env).clone());
        }

        for (fv, ty) in &let_decl.unbound_effects {
            env = env.add(Some(fv.clone()), ty.eval(&env).clone());
        }

        let mut binders = Default::default();
        let mut elab_binders = Vec::new();

        for (binder, ty) in self.binders.iter().zip(let_decl.args.iter()) {
            let pat = binder
                .pattern
                .check(ty.eval(&env), (ctx, &mut binders, env.clone()));

            elab_binders.push((pat, ty.clone()));
        }

        for binder in binders {
            env.add_var(binder.0, binder.1);
        }

        let ty = let_decl.ret.clone();

        let eval = let_decl.ambient.eval(&env);

        let binders = elab_binders;

        let effects = let_decl
            .ambient
            .eval(&env)
            .effect_row_set()
            .into_keys()
            .collect();

        
        ctx.errored = false;
        
        let body = self.body.check(ty.clone(), (ctx, eval, env.clone()));
        
        let types = ty.arrow_spine();
        
        if !ctx.errored {
            let problem = Problem::exhaustiveness(&body, types);
            let patterns = &self.body.last().unwrap().patterns;

            if patterns.first().is_some() {
                env.on(patterns
                    .first()
                    .unwrap()
                    .span
                    .clone()
                    .mix(patterns.last().unwrap().span.clone()));

                if let Witness::NonExhaustive(case) = problem.exaustive(ctx, env.clone()) {
                    ctx.report(&env, TypeErrorKind::NonExhaustive(case));
                };
            }
        }

        ctx.elaborated.lets.insert(
            self.name.clone(),
            elaborated::LetDecl {
                binders,
                effects,
                body,
            },
        );
    }
}

impl Declare for ModuleDecl {
    fn declare(&self, (ctx, env): (&mut Context, Env)) {
        self.decls.declare((ctx, env));
    }

    fn define(&self, (ctx, env): (&mut Context, Env)) {
        self.decls.define((ctx, env));
    }
}

impl Declare for Program {
    fn declare(&self, (ctx, env): (&mut Context, Env)) {
        self.modules.declare((ctx, env.clone()));
        self.types.declare((ctx, env.clone()));
        self.effects.declare((ctx, env.clone()));
        self.lets.declare((ctx, env.clone()));
        self.externals.declare((ctx, env));
    }

    fn define(&self, (ctx, env): (&mut Context, Env)) {
        self.modules.define((ctx, env.clone()));
        self.types.define((ctx, env.clone()));
        self.effects.define((ctx, env.clone()));
        self.lets.define((ctx, env.clone()));
        self.externals.define((ctx, env));
    }
}

pub struct TypeEnv {
    pub context: Context,
    pub env: Env,
}

impl TypeEnv {
    pub fn declare(&mut self, program: &Program) -> &mut Self {
        program.declare((&mut self.context, self.env.clone()));
        self
    }

    pub fn define(&mut self, program: &Program) -> &mut Self {
        program.define((&mut self.context, self.env.clone()));
        self
    }

    pub fn output(&mut self) -> elaborated::Program<Type<Real>> {
        std::mem::take(&mut self.context.elaborated)
    }
}

pub fn type_checker(reporter: Report) -> TypeEnv {
    let context = Context::new(reporter);
    let env = Env::default();

    TypeEnv { context, env }
}
