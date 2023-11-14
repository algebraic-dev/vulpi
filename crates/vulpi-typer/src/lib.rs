//! This is the entrypoint for the `vulpi-typer` crate. It is responsible for type checking a
//! higher rank, higher kinded, algebraic type system. It is also responsible for type inference
//! and type checking of the ambient effects system.

pub use context::Context;
use coverage::{Problem, Witness};
use errors::TypeErrorKind;
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
    r#abstract::{ExtDecl, ModuleDecl, Program, TypeDecl, TypeDef},
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
                binders: names.into_iter().zip(binders).collect(),
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

impl Declare for ExtDecl {
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

impl Declare for LetDecl {
    fn declare(&self, (ctx, mut env): (&mut Context, Env)) {
        let mut fvs = self
            .ret
            .as_ref()
            .map(|x| x.data.free_variables())
            .unwrap_or_default();

        for arg in &self.binders {
            fvs.extend(arg.ty.data.free_variables());
        }

        let mut unbound = Vec::new();

        for fv in fvs {
            let ty = ctx.hole(&env, Type::typ());
            env = env.add(Some(fv.clone()), ty.clone());
            unbound.push((fv, ty.quote(env.level)));
        }

        let mut args = Vec::new();

        for arg in &self.binders {
            let (ty, kind) = arg.ty.infer((ctx, env.clone()));
            env.on(arg.ty.span.clone());

            ctx.subsumes(env.clone(), kind, Kind::typ());

            args.push(ty);
        }

        let ret = if let Some(ret) = &self.ret {
            let (ty, kind) = ret.infer((ctx, env.clone()));
            env.on(ret.span.clone());
            ctx.subsumes(env.clone(), kind, Kind::typ());

            ty
        } else {
            ctx.hole(&env, Kind::typ())
        };

        let func_args = args.clone();

        let mut typ = Type::<Real>::function(args.clone(), ret.clone());
        
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

        let mut binders = Default::default();
        let mut elab_binders = Vec::new();

        for (binder, ty) in self.binders.iter().zip(let_decl.args.iter()) {
            let pat = binder
                .pat
                .check(ty.eval(&env), (ctx, &mut binders, env.clone()));

            elab_binders.push((pat, ty.clone()));
        }

        for binder in binders {
            env.add_var(binder.0, binder.1);
        }

        let ty = let_decl.ret.clone();


        let binders = elab_binders;

        ctx.errored = false;
        
        let body = self.body.check(ty.clone(), (ctx, env.clone()));
        
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

pub struct Programs(pub Vec<Program>);

impl Declare for Programs {
    fn declare(&self, (ctx, env): (&mut Context, Env)) {
        for program in self.0.iter() {
            program.modules.declare((ctx, env.clone()));
        }

        for program in self.0.iter() {
            program.types.declare((ctx, env.clone()));
        }

        for program in self.0.iter() {
            program.lets.declare((ctx, env.clone()));
        }

        for program in self.0.iter() {
            program.externals.declare((ctx, env.clone()));
        }
    }

    fn define(&self, (ctx, env): (&mut Context, Env)) {
        for program in self.0.iter() {
            program.types.define((ctx, env.clone()));
        }

        for program in self.0.iter() {
            program.lets.define((ctx, env.clone()));
        }

        for program in self.0.iter() {
            program.externals.define((ctx, env.clone()));
        }

        for program in self.0.iter() {
            program.modules.define((ctx, env.clone()));
        }
    }
}

impl Declare for Program {
    fn declare(&self, (ctx, env): (&mut Context, Env)) {
        self.modules.declare((ctx, env.clone()));
        self.types.declare((ctx, env.clone()));
        self.lets.declare((ctx, env.clone()));
        self.externals.declare((ctx, env));
    }

    fn define(&self, (ctx, env): (&mut Context, Env)) {
        self.modules.define((ctx, env.clone()));
        self.types.define((ctx, env.clone()));
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
