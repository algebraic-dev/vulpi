use std::collections::HashSet;

use vulpi_intern::Symbol;
use vulpi_syntax::{
    elaborated::{self},
    r#abstract::{
        LetBinder, Qualified, TraitDecl, {ExtDecl, LetDecl, TypeDef}, {Program, TypeDecl},
    },
};

use crate::{
    check::Check,
    context::Context,
    coverage::{Problem, Witness},
    errors::TypeErrorKind,
    eval::Eval,
    eval::Quote,
    infer::Infer,
    module::{Def, LetDef, TraitData, TypeData},
    r#virtual::Virtual,
    real::{Forall, Real},
    Env, Index, Kind, Type,
};

fn free_variables(let_sig: &vulpi_syntax::r#abstract::LetSignature) -> HashSet<Symbol> {
    let mut fvs = let_sig
        .ret
        .as_ref()
        .map(|x| x.data.free_variables())
        .unwrap_or_default();

    for arg in &let_sig.binders {
        fvs.extend(arg.typ().data.free_variables());
    }

    fvs
}

/// Trait for declaration of top level items inside the type checker.
pub trait Declare {
    type Return;

    fn declare(&self, ctx: (&mut Context, Env));
    fn define(&self, ctx: (&mut Context, Env)) -> Self::Return;
}

impl<T: Declare> Declare for Vec<T> {
    type Return = Vec<T::Return>;

    fn declare(&self, context: (&mut Context, Env)) {
        for decl in self {
            decl.declare((context.0, context.1.clone()));
        }
    }

    fn define(&self, context: (&mut Context, Env)) -> Self::Return {
        let mut ret = Vec::new();

        for decl in self {
            ret.push(decl.define((context.0, context.1.clone())));
        }

        ret
    }
}

impl<T: Declare> Declare for Option<T> {
    type Return = Option<T::Return>;

    fn declare(&self, context: (&mut Context, Env)) {
        if let Some(decl) = self {
            decl.declare(context);
        }
    }

    fn define(&self, context: (&mut Context, Env)) -> Self::Return {
        if let Some(decl) = self {
            Some(decl.define(context))
        } else {
            None
        }
    }
}

impl Declare for TraitDecl {
    type Return = ();

    fn declare(&self, (ctx, mut env): (&mut Context, Env)) {        
        env.set_current_span(self.span.clone());
        let vec = &self.binders;

        let mut names = Vec::new();
        let mut binders = Vec::new();
        let mut fvs = Vec::new();

        for binder in vec {
            let (n, binder) = binder.infer((ctx, env.clone()));
            let value = binder.eval(&env);
            binders.push(value.clone());
            names.push(n.clone());
            env = env.add(Some(n), value);
        }

        fvs.extend(self.binders.iter().map(|x| x.name().clone()));

        let kind = Type::<Virtual>::function(binders.clone(), Type::constraint());

        let mut supers = vec![];

        ctx.modules.get(&self.name.path).types.insert(
            self.name.name.clone(),
            TypeData {
                kind: kind.clone(),
                binders: names.into_iter().zip(binders.clone()).collect(),
                module: self.namespace.clone(),
                def: Def::Constraint,
            },
        );

        for super_ in &self.supers {
            env.set_current_span(super_.span.clone());
            let (value, typ) = super_.infer((ctx, env.clone()));
            ctx.subsumes(env.clone(), typ, Type::constraint());
            supers.push(value);
        }

        let mut signatures = Vec::new();

        for let_signature in &self.body {
            let mut env = env.clone();

            let free_variables = &free_variables(let_signature);
            let diff = HashSet::from_iter(fvs.iter().cloned());
            let signature_fvs = free_variables.difference(&diff);

            let mut unbound = Vec::new();

            for fv in fvs.iter() {
                let typ = ctx.hole(&env, Type::typ());
                env = env.add(Some(fv.clone()), typ.clone());
                unbound.push((fv, typ.quote(env.level)))
            }            

            for fv in signature_fvs {
                let typ = ctx.hole(&env, Type::typ());
                env = env.add(Some(fv.clone()), typ.clone());
                unbound.push((fv, typ.quote(env.level)))
            }

            let mut args = Vec::new();

            for arg in &let_signature.binders {
                let (typ, kind) = arg.typ().infer((ctx, env.clone()));
                env.set_current_span(arg.typ().span.clone());
                ctx.subsumes(env.clone(), kind, Kind::typ());    
                args.push(typ);
            }

            let ret = if let Some(ret) = &let_signature.ret {
                let (typ, kind) = ret.infer((ctx, env.clone()));
                env.set_current_span(ret.span.clone());
                ctx.subsumes(env.clone(), kind, Kind::typ());
    
                typ
            } else {
                ctx.hole(&env, Kind::typ())
            };
            
            let mut typ = Type::<Real>::function(args.clone(), ret.clone());
    
            let fvs = fvs.iter().map(|x| {
                let Some((index, _, _)) = env.find(x) else { unreachable!() };
                Type::bound(Index(index))
            }).collect();

            let constraint = Type::<Real>::application(Type::variable(self.name.clone()), fvs);

            typ = Type::qualified(constraint, typ);

            for (name, kind) in unbound.iter().rev().cloned() {
                typ = Type::forall(Forall {
                    name: name.clone(),
                    kind,
                    body: typ,
                });
            }

            signatures.push((self.name.clone(), typ));
        }

        ctx.modules.get(&self.name.path).traits.insert(
            self.name.name.clone(),
            TraitData {
                kind,
                binders,
                supers,
                signatures,
            },
        );
    }

    fn define(&self, _context: (&mut Context, Env)) -> Self::Return {}
}

impl Declare for TypeDecl {
    type Return = (Qualified, elaborated::TypeDecl);

    fn declare(&self, (ctx, env): (&mut Context, Env)) {
        let vec = &self.binders;

        let mut names = Vec::new();
        let mut binders = Vec::new();

        for binder in vec {
            let (n, binder) = binder.infer((ctx, env.clone()));
            binders.push(binder.eval(&env));
            names.push(n);
        }

        let kind = Type::<Virtual>::function(binders.clone(), Type::typ());

        let type_def = &self.def;
        let def = get_definition_of_type(type_def);

        ctx.modules.get(&self.name.path).types.insert(
            self.name.name.clone(),
            TypeData {
                kind,
                binders: names.into_iter().zip(binders).collect(),
                module: self.namespace.clone(),
                def,
            },
        );
    }

    fn define(&self, (ctx, mut env): (&mut Context, Env)) -> Self::Return {
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
                        env.set_current_span(arg.span.clone());
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
                    env.set_current_span(field.1.span.clone());

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

        (self.name.clone(), decl)
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

impl Declare for ExtDecl {
    type Return = (Qualified, elaborated::ExternalDecl<Type<Real>>);

    fn declare(&self, (ctx, mut env): (&mut Context, Env)) {
        let fvs = self.typ.data.free_variables();

        let start_env = env.clone();

        let mut unbound = Vec::new();

        for fv in fvs {
            let typ = ctx.hole(&env, Type::typ());
            env = env.add(Some(fv.clone()), typ.clone());
            unbound.push((fv, typ.quote(env.level)))
        }

        let (typ, k) = self.typ.infer((ctx, env.clone()));
        ctx.subsumes(env.clone(), k, Kind::typ());

        let typ = typ.eval(&start_env);

        ctx.modules.get(&self.namespace).variables.insert(
            self.name.name.clone(),
            LetDef {
                typ: typ.clone(),
                unbound,
                ret: typ.clone(),
                args: vec![],
            },
        );

        ctx.elaborated.externals.insert(
            self.name.clone(),
            elaborated::ExternalDecl {
                name: self.name.clone(),
                typ: typ.quote(env.level),
                binding: self.ret.clone(),
            },
        );
    }

    fn define(&self, context: (&mut Context, Env)) -> Self::Return {
        (
            self.name.clone(),
            context
                .0
                .elaborated
                .externals
                .get(&self.name)
                .unwrap()
                .clone(),
        )
    }
}

impl Declare for LetDecl {
    type Return = (Qualified, elaborated::LetDecl<Type<Real>>);

    fn declare(&self, (ctx, mut env): (&mut Context, Env)) {
        let start_env = env.clone();

        let mut fvs = self
            .signature
            .ret
            .as_ref()
            .map(|x| x.data.free_variables())
            .unwrap_or_default();

        for arg in &self.signature.binders {
            fvs.extend(arg.typ().data.free_variables());
        }

        let mut unbound = Vec::new();

        for fv in fvs {
            let typ = ctx.hole(&env, Type::typ());
            env = env.add(Some(fv.clone()), typ.clone());
            unbound.push((fv, typ.quote(env.level)));
        }

        let mut args = Vec::new();

        for arg in &self.signature.binders {
            let (typ, kind) = arg.typ().infer((ctx, env.clone()));
            env.set_current_span(arg.typ().span.clone());

            ctx.subsumes(env.clone(), kind, Kind::typ());

            args.push(typ);
        }

        let ret = if let Some(ret) = &self.signature.ret {
            let (typ, kind) = ret.infer((ctx, env.clone()));
            env.set_current_span(ret.span.clone());
            ctx.subsumes(env.clone(), kind, Kind::typ());

            typ
        } else {
            ctx.hole(&env, Kind::typ())
        };

        let func_args = args.clone();

        let mut typ = Type::<Real>::function(args.clone(), ret.clone());

        for (name, kind) in unbound.iter().rev().cloned() {
            typ = Type::forall(Forall {
                name,
                kind,
                body: typ,
            });
        }

        ctx.modules
            .get(&self.signature.name.path.clone())
            .variables
            .insert(
                self.signature.name.name.clone(),
                LetDef {
                    typ: typ.eval(&start_env),
                    unbound,
                    ret: ret.eval(&env),
                    args: func_args,
                },
            );
    }

    fn define(&self, (ctx, mut env): (&mut Context, Env)) -> Self::Return {
        env.set_current_span(self.signature.span.clone());

        let let_decl = ctx.modules.let_decl(&self.signature.name).clone();

        for (fv, typ) in &let_decl.unbound {
            env = env.add(Some(fv.clone()), typ.eval(&env).clone());
        }

        let mut binders = Default::default();
        let mut elab_binders = Vec::new();

        for (binder, typ) in self.signature.binders.iter().zip(let_decl.args.iter()) {
            if let LetBinder::Param(binder) = binder {
                let pat = binder
                    .pat
                    .check(typ.eval(&env), (ctx, &mut binders, env.clone()));

                elab_binders.push((pat, typ.clone()));
            }
        }

        for binder in binders {
            env.add_var(binder.0, binder.1);
        }

        let typ = let_decl.ret.clone();
        let binders = elab_binders;

        ctx.errored = false;

        let body = self.body.check(typ.clone(), (ctx, env.clone()));
        let types = typ.arrow_spine();

        if !ctx.errored {
            let problem = Problem::exhaustiveness(&body, types);
            let patterns = &self.body.last().unwrap().patterns;

            if patterns.first().is_some() {
                env.set_current_span(patterns
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

        (
            self.signature.name.clone(),
            elaborated::LetDecl {
                name: self.signature.name.clone(),
                binders,
                body,
                constants: self.constant.clone(),
            },
        )
    }
}

pub struct Programs(pub Vec<Program>);

impl Declare for Programs {
    type Return = Vec<elaborated::Program<Type<Real>>>;

    fn declare(&self, (ctx, env): (&mut Context, Env)) {
        for program in self.0.iter() {
            program.types.declare((ctx, env.clone()));
        }

        for program in self.0.iter() {
            program.lets.declare((ctx, env.clone()));
        }

        for program in self.0.iter() {
            program.externals.declare((ctx, env.clone()));
        }

        for program in self.0.iter() {
            program.traits.declare((ctx, env.clone()));
        }
    }

    fn define(&self, (context, env): (&mut Context, Env)) -> Self::Return {
        let mut programs = vec![elaborated::Program::default(); self.0.len()];

        for (i, program) in self.0.iter().enumerate() {
            let typ = program.types.define((context, env.clone()));
            programs[i].types = typ.into_iter().collect();
        }

        for (i, program) in self.0.iter().enumerate() {
            let let_decl = program.lets.define((context, env.clone()));
            programs[i].lets = let_decl.into_iter().collect();
        }

        for (i, program) in self.0.iter().enumerate() {
            let ext_decl = program.externals.define((context, env.clone()));
            programs[i].externals = ext_decl.into_iter().collect();
        }

        for (i, program) in self.0.iter().enumerate() {
            let _trait_decl = program.traits.define((context, env.clone()));
            programs[i].commands = program.commands.clone();
        }

        programs
    }
}
