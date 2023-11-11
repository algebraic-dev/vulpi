//! The resolver is responsible for taking a single concrete tree and turn it into an abstract
//! syntax tree with all the names resolved.

use std::cell::Ref;
use std::collections::HashSet;
use std::{cell::RefCell, rc::Rc};

use im_rc::HashMap;
use vulpi_intern::Symbol;

use vulpi_location::Spanned;
use vulpi_syntax::concrete::tree::LetMode;
use vulpi_syntax::concrete::{self, tree};
use vulpi_syntax::r#abstract as abs;
use vulpi_syntax::r#abstract::Visibility;

pub struct Path {
    pub is_self: bool,
    pub segments: Vec<Symbol>,
}

pub struct Crate {
    pub name: Symbol,
}

pub enum DefinitionKind {
    Type,
    Value,
    Module,
}

pub enum ScopeKind {
    Module,
    Local,
}

#[derive(Default)]
pub struct Bag<V> {
    pub types: V,
    pub values: V,
    pub modules: V,
}

pub struct Namespace {
    pub kind: ScopeKind,
    pub name: Symbol,

    pub parent: Option<Scope>,
    pub locals: Bag<HashSet<Symbol>>,

    pub declared: Bag<HashMap<Symbol, abs::Visibility>>,
    pub aliases: Bag<HashMap<Symbol, Path>>,

    pub submodules: Vec<Scope>,

    pub opened: Vec<Scope>,
}

#[derive(Clone)]
pub struct Scope(Rc<RefCell<Namespace>>);

impl Scope {
    pub fn new(kind: ScopeKind, name: Symbol) -> Scope {
        Scope(Rc::new(RefCell::new(Namespace {
            name,
            declared: Default::default(),
            locals: Default::default(),
            aliases: Default::default(),
            submodules: Default::default(),
            opened: Default::default(),
            parent: None,
            kind,
        })))
    }
}

impl Scope {
    pub fn borrow(&self) -> Ref<Namespace> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> std::cell::RefMut<Namespace> {
        self.0.borrow_mut()
    }

    /// Defines a name in the current namespace. It takes the visibility of the definition, the
    /// kind of the definition, and the name of the definition.
    pub fn define<Vis: Into<abs::Visibility>>(&self, kind: DefinitionKind, vis: Vis, name: Symbol) {
        let bag = &mut self.borrow_mut().declared;

        match kind {
            DefinitionKind::Type => bag.types.insert(name, vis.into()),
            DefinitionKind::Value => bag.values.insert(name, vis.into()),
            DefinitionKind::Module => bag.modules.insert(name, vis.into()),
        };
    }

    pub fn fork(&self, kind: ScopeKind, name: Symbol) -> Scope {
        let value = Scope::new(kind, name);
        self.borrow_mut().submodules.push(value.clone());
        value
    }
}

pub struct Solver<T> {
    run: Box<dyn FnOnce(Scope) -> T>,
}

impl<T: 'static> Solver<T> {
    pub fn new(run: impl FnOnce(Scope) -> T + 'static) -> Self {
        Self { run: Box::new(run) }
    }

    pub fn map<U: 'static>(self, f: impl FnOnce(T) -> U + 'static) -> Solver<U> {
        Solver::new(move |namespace| f((self.run)(namespace)))
    }

    pub fn eval(self, namespace: Scope) -> T {
        (self.run)(namespace)
    }
}

/// Top level declarations are the ones that can be declared at the top level of a module.
pub mod top_level {
    use super::*;

    /// Resolve a top level declaration and returns the solver for it.
    pub fn resolve(module: Scope, top_level: tree::TopLevel) -> Option<Solver<abs::TopLevel>> {
        use concrete::tree::TopLevel::*;

        match top_level {
            Let(let_decl) => Some(resolve_let(module, *let_decl).map(abs::TopLevel::Let)),
            Type(type_decl) => Some(resolve_type_decl(module, *type_decl).map(abs::TopLevel::Type)),
            Module(mod_decl) => Some(resolve_module(module, *mod_decl).map(abs::TopLevel::Module)),
            External(ext) => Some(resolve_external(module, *ext).map(abs::TopLevel::External)),
            Effect(eff_decl) => {
                Some(resolve_effect_decl(module, *eff_decl).map(abs::TopLevel::Effect))
            }
            Use(use_decl) => {
                declare_use(module, *use_decl);
                None
            }
            Error(_) => None,
        }
    }

    /// Resolve an effect declaration and returns the solver for it. It does fork the namespace
    /// for the effect, and declares the field names' symbols in the submodule.
    pub fn resolve_effect_decl(module: Scope, decl: tree::EffectDecl) -> Solver<abs::EffectDecl> {
        /// Transform an effect field into an abstract effect field.
        fn transform_effect_field(module: &Scope, field: tree::EffectField) -> abs::EffectField {
            todo!()
        }

        let name = decl.name.symbol();
        let submodule = module.fork(ScopeKind::Module, decl.name.symbol());

        module.define(DefinitionKind::Type, decl.visibility.clone(), name.clone());

        for field in &decl.fields {
            let visibility = field.visibility.clone();
            let name = field.name.symbol();
            submodule.define(DefinitionKind::Value, visibility, name);
        }

        Solver::new(move |module| abs::EffectDecl {
            name,
            visibility: decl.visibility.into(),
            binders: decl
                .binders
                .into_iter()
                .map(|x| transform_type_binder(&module, x))
                .collect(),
            effects: decl
                .fields
                .into_iter()
                .map(|x| transform_effect_field(&module, x))
                .collect(),
        })
    }

    /// Resolve a let declaration and returns the solver for it.
    pub fn resolve_let(module: Scope, decl: tree::LetDecl) -> Solver<abs::LetDecl> {
        let name = decl.name.symbol();

        // Gets the location of the name, so we can present the errors in a less annoying way
        // in the IDE.
        let span = decl.name.0.value.span.clone();

        module.define(DefinitionKind::Value, decl.visibility.clone(), name.clone());

        Solver::new(move |module| abs::LetDecl {
            span,
            name,
            visibility: decl.visibility.into(),
            body: pattern::transform_let_mode(&module, decl.body),
            ret: decl.ret.map(|(_, effects, type_kind)| {
                let effects = effects.map(|x| transform_effect(&module, x));
                let type_kind = transform_type(&module, *type_kind);
                (effects, type_kind)
            }),
            binders: decl
                .binders
                .into_iter()
                .map(|x| transform_binder(&module, x))
                .collect(),
        })
    }

    /// Resolve a type declaration and returns the solver for it.
    pub fn resolve_type_decl(module: Scope, decl: tree::TypeDecl) -> Solver<abs::TypeDecl> {
        let name = decl.name.symbol();
        let submodule = module.fork(ScopeKind::Module, decl.name.symbol());

        module.define(DefinitionKind::Type, decl.visibility.clone(), name.clone());

        match decl.def {
            None => {}
            Some((_, tree::TypeDef::Record(record))) => {
                for (field, _) in &record.fields {
                    let name = field.name.symbol();
                    let vis = into_field_visiblity(field.visibility.clone().into());
                    submodule.define(DefinitionKind::Value, vis, name);
                }
            }
            Some((_, tree::TypeDef::Sum(sum))) => {
                for cons in &sum.constructors {
                    let name = cons.name.symbol();
                    submodule.define(DefinitionKind::Value, Visibility::Public, name);
                }
            }
            Some((_, tree::TypeDef::Synonym(synonym))) => {
                todo!()
            }
        }

        Solver::new(|module| todo!())
    }

    /// Resolve an external declaration and returns the solver for it.
    pub fn resolve_external(module: Scope, decl: tree::ExtDecl) -> Solver<abs::ExtDecl> {
        let name = decl.name.symbol();

        module.define(DefinitionKind::Value, decl.visibility.clone(), name.clone());

        Solver::new(move |module| abs::ExtDecl {
            name,
            visibility: decl.visibility.into(),
            ty: transform_type(&module, *decl.typ),
            ret: decl.str.symbol(),
        })
    }

    /// Resolve a module declaration and returns the solver for it.
    pub fn resolve_module(namespace: Scope, decl: tree::ModuleDecl) -> Solver<abs::ModuleDecl> {
        Solver::new(|module| todo!())
    }

    pub fn declare_use(module: Scope, decl: tree::UseDecl) {}
}

/// Patterns are the ones that can be used in a match expression.
pub mod pattern {
    use super::*;

    /// Transform a pattern into an abstract pattern.
    pub fn transform(module: &Scope, pattern: tree::Pattern) -> abs::Pattern {
        todo!()
    }

    /// Transform a pattern into an abstract pattern.
    pub fn transform_pattern_arm(module: &Scope, arm: tree::PatternArm) -> abs::PatternArm {
        abs::PatternArm {
            patterns: arm
                .patterns
                .into_iter()
                .map(|x| pattern::transform(module, *x.0))
                .collect(),
            expr: expr::transform(module, *arm.expr),
            guard: arm.guard.map(|x| expr::transform(module, *x.1)),
        }
    }

    /// Transform a let mode into a list of pattern arms.
    pub fn transform_let_mode(module: &Scope, mode: LetMode) -> Vec<abs::PatternArm> {
        match mode {
            LetMode::Body(_, expr) => {
                vec![abs::PatternArm {
                    patterns: vec![],
                    expr: expr::transform(module, *expr),
                    guard: None,
                }]
            }
            LetMode::Cases(cases) => cases
                .into_iter()
                .map(|x| transform_pattern_arm(module, x.arm))
                .collect(),
        }
    }
}

/// Expressions are the ones that can be used in a function body.
pub mod expr {
    use super::*;

    pub fn transform(namespace: &Scope, expr: concrete::tree::Expr) -> abs::Expr {
        todo!()
    }
}

/// The super module can access all the names in the module of an struct, so this is useful
/// to avoid having to pass the module around.
pub fn into_field_visiblity(vis: abs::Visibility) -> abs::Visibility {
    match vis {
        abs::Visibility::Public => abs::Visibility::Public,
        abs::Visibility::Private => abs::Visibility::Super,
        abs::Visibility::Super => abs::Visibility::Super,
    }
}

pub fn transform_kind(kind: tree::Kind) -> abs::Kind {
    let data = match kind.data {
        tree::KindType::Star(_) => abs::KindType::Star,
        tree::KindType::Variable(x) => {
            match x.symbol().get().as_str() {
                "Effect" => abs::KindType::Effect,
                "Type" => abs::KindType::Star,
                "Constraint" => abs::KindType::Constraint,
                _ => todo!("add error that the kind is not recognized")
            }
        },
        tree::KindType::Arrow(x, _, y) => abs::KindType::Arrow(
            transform_kind(*x),
            transform_kind(*y),
        ),
        tree::KindType::Parenthesis(x) => {
            return transform_kind(*x.data)
        },
    };

    Box::new(Spanned {
        data,
        span: kind.span.clone(),
    })
}

pub fn transform_type_binder(module: &Scope, binder: tree::TypeBinder) -> abs::TypeBinder {
    match binder {
        tree::TypeBinder::Implicit(x) => abs::TypeBinder::Implicit(x.symbol()),
        tree::TypeBinder::Explicit(t) => abs::TypeBinder::Explicit(t.data.name.symbol(), transform_kind(*t.data.kind))
    }
}

pub fn transform_type(module: &Scope, concrete_type: tree::Type) -> abs::Type {
    todo!()
}

pub fn transform_effect(module: &Scope, eff: tree::Effects) -> abs::Effects {
    todo!()
}

pub fn transform_binder(module: &Scope, binder: tree::Binder) -> abs::Binder {
    todo!()
}

pub fn transform_sttm(namespace: &Scope, sttm: concrete::tree::Sttm) -> abs::Sttm {
    todo!()
}
