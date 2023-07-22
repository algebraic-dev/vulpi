//! This module is useful to declare types and values in the environment in order to be able to
//! create mutually recursive types and values.

use std::rc::Rc;

use crate::infer::Infer;
use im_rc::HashSet;

use vulpi_location::Spanned;
use vulpi_storage::interner::Symbol;
use vulpi_syntax::resolved::{Program, TypeDef, TypeKind};

use crate::context::Env;
use crate::types::{free_variables_located, KindType, Mono, Scheme, Type};
use crate::unify::{self};
use crate::Modules;

/// Declare all types in the environment.
// TODO: Improve kind inference.
pub fn declare_types(modules: &mut Modules, program: &Program) {
    for typ in &program.types {
        // TODO: Check if parameters are unique.

        let name = typ.name.clone();
        let values = make_kind_function(&typ.params);
        modules.declare_type(program.id, name.data, values);
    }
}

/// This function declares all variants and let type.
pub fn declare_values_types(env: Env, program: &Program) {
    for typ in &program.types {
        declare_variants(&env, typ, program);
    }

    for let_ in &program.lets {
        declare_let_types(&env, let_, program);
    }
}

fn declare_variants(env: &Env, typ: &vulpi_syntax::resolved::TypeDecl, program: &Program) {
    let mut env = env.clone();

    let params: Vec<_> = typ
        .params
        .iter()
        .enumerate()
        .map(|(i, l)| Type::new(Mono::Generalized(i, l.data.clone())))
        .collect();

    for (i, params) in typ.params.iter().enumerate() {
        env.type_variables
            .insert(params.data.clone(), (Rc::new(KindType::Star), i));
    }

    let init = Type::new(Mono::Variable(program.id, typ.name.data.clone()));
    let ret_type = make_application(params, init);
    let variables: Vec<_> = typ.params.iter().map(|x| x.data.clone()).collect();

    declare_type_def(typ, env, ret_type, variables);
}

fn declare_let_types(env: &Env, let_: &vulpi_syntax::resolved::LetDecl, program: &Program) {
    let mut env = env.clone();
    let mut fvs = HashSet::new();
    let name = let_.name.clone();

    for param in &let_.params {
        free_variables_located(env.clone(), &param.1, &mut fvs);
    }

    for (i, var) in fvs.iter().enumerate() {
        env.type_variables
            .insert(var.clone(), (Rc::new(KindType::Star), i));
    }

    let ret = if let Some(typ) = &let_.ret {
        let (kind, typ) = typ.infer(env.clone());
        unify::unify_kinds(env.clone(), kind, Rc::new(KindType::Star));
        typ
    } else {
        env.new_hole()
    };

    let typ = infer_types(let_.params.iter().map(|x| &x.1), &env);
    let typ = make_function(typ, &ret);
    let value = Scheme::new(fvs.into_iter().collect(), typ);

    env.modules
        .borrow_mut()
        .declare_let(program.id, name.data, value);
}

fn declare_type_def(
    typ: &vulpi_syntax::resolved::TypeDecl,
    env: Env,
    ret_type: Rc<Mono>,
    variables: Vec<Symbol>,
) {
    match &typ.def {
        TypeDef::Enum(enum_) => {
            for variant in &enum_.variants {
                let name = variant.name.clone();

                let args = &variant.args;
                let types: Vec<_> = infer_types(args.iter(), &env);
                let monotype = make_function(types, &ret_type);
                let value = Scheme::new(variables.clone(), monotype);

                env.modules
                    .borrow_mut()
                    .declare_cons(typ.id, name.data, value);
            }
        }
        TypeDef::Record(rec_) => {
            for field in &rec_.fields {
                let name = field.name.clone();
                let (kind, field_typ) = field.ty.infer(env.clone());

                unify::unify_kinds(env.clone(), kind, Rc::new(KindType::Star));

                let monotype = Type::new(Mono::Function(ret_type.clone(), field_typ));
                let value = Scheme::new(variables.clone(), monotype);

                env.modules
                    .borrow_mut()
                    .declare_field(typ.id, name.data, value);
            }
        }
        TypeDef::Synonym(_) => todo!(),
    }
}

fn infer_types<'a, I: Iterator<Item = &'a Spanned<TypeKind>>>(args: I, env: &Env) -> Vec<Rc<Mono>> {
    args.map(|x| {
        let (kind, ty) = x.infer(env.clone());
        unify::unify_kinds(env.clone(), kind, Rc::new(KindType::Star));
        ty
    })
    .collect()
}

fn make_kind_function(values: &[Spanned<Symbol>]) -> Rc<KindType> {
    values
        .iter()
        .map(|_| Rc::new(KindType::Star))
        .rfold(Rc::new(KindType::Star), |x, y| Rc::new(KindType::Fun(y, x)))
}

fn make_function(types: Vec<Rc<Mono>>, ret_type: &Rc<Mono>) -> Rc<Mono> {
    types
        .into_iter()
        .rfold(ret_type.clone(), |x, y| Type::new(Mono::Function(y, x)))
}

fn make_application(params: Vec<Rc<Mono>>, init: Rc<Mono>) -> Rc<Mono> {
    params
        .iter()
        .fold(init, |x, y| Type::new(Mono::Application(x, y.clone())))
}
