use std::collections::HashMap;

use context::Env;
use infer::Infer;
use types::{Kind, Mono, Scheme, Type};
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;
use vulpi_syntax::resolved::{Program, TypeDef};

pub mod check;
pub mod context;
pub mod error;
pub mod infer;
pub mod types;
pub mod unify;

#[derive(PartialEq, Eq, Hash)]
pub enum Data {
    Let(Symbol),
    Constructor(Symbol),
    Field(Symbol),
}

#[derive(Default)]
pub struct Interface {
    pub types: HashMap<Symbol, Kind>,
    pub values: HashMap<Data, types::Scheme>,
}

#[derive(Default)]
pub struct Modules {
    pub modules: HashMap<Id<id::Namespace>, Interface>,
}

pub fn declare_types(modules: &mut Modules, program: &Program) {
    for typ in &program.types {
        let name = typ.name.clone();

        let values = typ
            .params
            .iter()
            .map(|_| Kind::Star)
            .fold(Kind::Star, |x, y| Kind::Fun(Box::new(y), Box::new(x)));

        modules
            .modules
            .entry(program.id)
            .or_default()
            .types
            .insert(name.data, values);
    }
}

pub fn declare_values(mut env: Env, program: &Program) {
    for typ in &program.types {
        let params: Vec<_> = (0..typ.params.len())
            .map(|i| Type::new(Mono::Generalized(i)))
            .collect();

        for (i, params) in typ.params.iter().enumerate() {
            env.type_variables
                .insert(params.data.clone(), (Kind::Star, i));
        }

        let init = Type::new(Mono::Variable(program.id, typ.name.data.clone()));

        let ret_type = params
            .iter()
            .fold(init, |x, y| Type::new(Mono::Application(x, y.clone())));

        let variables: Vec<_> = typ.params.iter().map(|x| x.data.clone()).collect();

        match &typ.def {
            TypeDef::Enum(enum_) => {
                for variant in &enum_.variants {
                    let name = variant.name.clone();

                    let types: Vec<_> = variant.args.iter().map(|x| x.infer(env.clone())).collect();

                    let monotype = types.into_iter().rfold(ret_type.clone(), |x, y| {
                        // TODO: Add unification of kinds here
                        Type::new(Mono::Function(y.1, x))
                    });

                    let value = Scheme {
                        variables: variables.clone(),
                        monotype,
                    };

                    println!("{}", value);

                    env.modules
                        .borrow_mut()
                        .modules
                        .entry(typ.id)
                        .or_default()
                        .values
                        .insert(Data::Constructor(name.data), value);
                }
            }
            TypeDef::Record(_) => todo!(),
            TypeDef::Synonym(_) => todo!(),
        }
    }
}
