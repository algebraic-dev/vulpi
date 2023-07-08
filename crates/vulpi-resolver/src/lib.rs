//! Module for module resolution and symbol resolution. It mutates the AST in order to resolve the
//! symbols to a global namespace. It also checks for unbound names, name collision and non linear
//! patterns and duplicated type variables.

use std::collections::HashMap;

use context::{to_namespace_path, Action, Context};

use error::ResolverError;
use scope::scopable::{self, Module, TypeVariable};
use vulpi_location::{Location, Spanned};
use vulpi_storage::namespace::{self, Name, OccMap, Path, Qualified};
use vulpi_syntax::r#abstract::*;

use vulpi_syntax::r#abstract as abs;

pub mod context;
pub mod error;
pub mod scope;

pub fn duplicated<U: Clone, T: Eq + std::hash::Hash, I: IntoIterator<Item = U>>(
    iter: I,
    fun: fn(U) -> T,
) -> HashMap<T, Vec<U>> {
    let mut entries = HashMap::new();

    for key in iter.into_iter() {
        let entry = entries.entry(fun(key.clone())).or_insert_with(Vec::new);
        entry.push(key);
    }

    entries.into_iter().filter(|(_, v)| v.len() > 1).collect()
}

pub fn get_path(qualified: &abs::Qualified) -> Option<Path> {
    if qualified.segments.is_empty() {
        return None;
    }

    Some(Path(
        qualified
            .segments
            .iter()
            .map(|x| x.0.clone())
            .collect::<Vec<_>>(),
    ))
}

pub fn to_namespace_qualified(qualified: &abs::Qualified) -> Option<Qualified> {
    if qualified.segments.is_empty() {
        return None;
    }

    Some(Qualified {
        path: to_namespace_path(&qualified),
        name: namespace::Name(qualified.segments.last().unwrap().0.clone()),
    })
}

pub trait Resolver {
    fn declare(&mut self, _ctx: &mut Context) {}

    fn resolve(&mut self, ctx: &mut Context);
}

impl Resolver for LetDecl {
    fn declare(&mut self, ctx: &mut Context) {
        ctx.declare_value(Name(self.name.0.clone()))
    }

    fn resolve(&mut self, ctx: &mut Context) {
        todo!()
    }
}

impl Resolver for TypeKind {
    fn resolve(&mut self, ctx: &mut Context) {
        match self {
            TypeKind::Upper(upper) => {
                let actual = ctx.actual();
                let file = actual.file_id;

                if let Some(name) = to_namespace_qualified(upper) {
                    let alias = actual.aliases.get(&name.path).unwrap_or(&name.path).clone();
                    let namespace = ctx.get_by_path(&alias);

                    if let Some(namespace) = namespace {
                        if namespace.contains_type(&name.name) {
                            upper.segments = namespace
                                .path
                                .0
                                .clone()
                                .iter()
                                .map(|x| Ident::generate(x.clone()))
                                .collect();
                        }
                    } else {
                        ctx.report(ResolverError {
                            location: Location::new(file, upper.range.clone()),
                            kind: error::ResolverErrorKind::UnboundType(name),
                        })
                    }
                } else if !ctx.actual().contains_type(&Name(upper.last.0.clone())) {
                    ctx.report(ResolverError {
                        location: Location::new(file, upper.range.clone()),
                        kind: error::ResolverErrorKind::UnboundType(Qualified {
                            path: Path(vec![]),
                            name: Name(upper.last.0.clone()),
                        }),
                    })
                }
            }
            TypeKind::Lower(s) => {
                let actual = ctx.actual();
                let file = actual.file_id;

                if !ctx.scopes.contains::<TypeVariable>(&Name(s.0.clone())) {
                    ctx.report(ResolverError {
                        location: Location::new(file, s.1.clone()),
                        kind: error::ResolverErrorKind::TypeVariableNotInScope(s.0.clone()),
                    })
                }
            }
            TypeKind::Arrow(s) => todo!(),
            TypeKind::Application(s) => todo!(),
            TypeKind::Forall(s) => todo!(),
            TypeKind::Unit => todo!(),
        }
    }
}

impl<T: Resolver> Resolver for Spanned<T> {
    fn resolve(&mut self, ctx: &mut Context) {
        self.data.resolve(ctx);
    }
}

impl Resolver for Constructor {
    fn declare(&mut self, ctx: &mut Context) {
        ctx.declare_value(Name(self.name.0.clone()))
    }

    fn resolve(&mut self, ctx: &mut Context) {
        ctx.scopes
            .add::<scopable::Constructor>(Name(self.name.0.clone()));

        for param in &mut self.args {
            param.declare(ctx);
        }
    }
}

impl Resolver for EnumDecl {
    fn declare(&mut self, ctx: &mut Context) {
        for variant in &mut self.constructors {
            variant.declare(ctx);
        }
    }

    fn resolve(&mut self, ctx: &mut Context) {
        for variant in &mut self.constructors {
            variant.resolve(ctx);
        }
    }
}

impl Resolver for TypeDef {
    fn declare(&mut self, ctx: &mut Context) {
        if let TypeDef::Enum(sum) = self {
            sum.declare(ctx)
        }
    }

    fn resolve(&mut self, ctx: &mut Context) {
        match self {
            TypeDef::Enum(sum) => sum.resolve(ctx),
            TypeDef::Record(_) => todo!(),
            TypeDef::Synonym(_) => todo!(),
        }
    }
}

impl Resolver for TypeDecl {
    fn declare(&mut self, ctx: &mut Context) {
        ctx.declare_type(Name(self.name.0.clone()));

        let current_path = ctx.actual().path.clone();
        let new_path = current_path.join([self.name.0.clone()].as_slice().into());

        let derived = ctx.actual().derive(new_path.clone());
        let id = ctx.declare_namespace(new_path, derived);

        self.def.declare(ctx);

        self.namespace = Some(id);

        ctx.unload();
    }

    fn resolve(&mut self, ctx: &mut Context) {
        ctx.enter(self.namespace.unwrap());

        let actual = ctx.actual();

        let duplicates = duplicated(self.params.clone(), |param| param.0);

        let file = actual.file_id;

        for duplicates in duplicates.values() {
            let fst = duplicates[0].clone();
            ctx.report(ResolverError {
                location: Location::new(file, fst.1.clone()),
                kind: error::ResolverErrorKind::DuplicatedTypeVariable(fst.0),
            })
        }

        ctx.scope::<Module, _>(|ctx| {
            for param in &mut self.params {
                ctx.scopes.add::<TypeVariable>(Name(param.0.clone()));
            }

            self.def.resolve(ctx);
        });

        ctx.unload();
    }
}

impl Resolver for UseDecl {
    fn declare(&mut self, ctx: &mut Context) {
        match &self.alias {
            Some(alias) => {
                ctx.load(self.path.range.clone(), to_namespace_path(&self.path));
                ctx.alias(to_namespace_path(&self.path), to_namespace_path(alias))
            }
            None => match ctx.load(self.path.range.clone(), to_namespace_path(&self.path)) {
                Action::Cached(id) | Action::Created(id) => {
                    let namespace = ctx.get(id);
                    let path = namespace.path.clone();
                    let declared = namespace.declared.clone();

                    let mapped = declared.map(|map| {
                        map.into_keys()
                            .map(|x| (x.clone(), Qualified::new(path.clone(), x)))
                            .collect::<OccMap<_>>()
                    });

                    ctx.actual().merge(mapped)
                }
                Action::Failed => (),
            },
        }
    }

    fn resolve(&mut self, _ctx: &mut Context) {
        unreachable!("use statements should not be resolved.")
    }
}

impl Resolver for Program {
    fn declare(&mut self, ctx: &mut Context) {
        for item in &mut self.lets {
            item.declare(ctx);
        }

        for item in &mut self.types {
            item.declare(ctx);
        }

        for item in &mut self.uses {
            item.declare(ctx);
        }

        use vulpi_tree::Show;

        println!("{}", self.show())
    }

    fn resolve(&mut self, ctx: &mut Context) {
        for item in &mut self.lets {
            item.resolve(ctx);
        }

        for item in &mut self.types {
            item.resolve(ctx);
        }

        self.uses = vec![];
    }
}
