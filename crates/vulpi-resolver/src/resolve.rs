//! Module to resolve symbols in a module. This is the third stage of the compiler pipeline and it
//! checks for the following:
//!
//! - All variables inside a pattern are linear
//!

use crate::ambiguity::DataType;
use crate::declare::Modules;
use crate::error::{ResolverError, ResolverErrorKind};
use crate::scopes::scopable::{self, TypeVariable, Variable};
use crate::scopes::{Kaleidoscope, Scopeable};

use std::collections::{HashMap, HashSet};
use std::ops::Range;

use vulpi_location::{Byte, Spanned};
use vulpi_report::{Diagnostic, Report};

use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;
use vulpi_syntax::resolved::{self};
use vulpi_syntax::{elaborated, r#abstract::*};

use super::ambiguity::ImportMap;

/// The resolver context. This is used to keep track of the symbols that are captured by patterns,
/// and to report errors.
pub struct Context<'a> {
    /// Pattern symbols captured by the pattern.
    captured: Vec<HashMap<Symbol, Range<Byte>>>,

    /// Error report structure
    reporter: Report,

    /// THe current file
    file: Id<id::File>,

    /// A collection of scopes that are currently active
    scopes: Kaleidoscope,

    /// The set of types that are defined in this scope
    modules: &'a Modules,

    /// Alias to import map
    aliases: HashMap<Symbol, Vec<Symbol>>,

    /// A bunch of things that got imported like constructors, values and types in the current scope
    available: ImportMap<resolved::Qualified>,

    /// Imported things in order to inject in the type checker
    imported: HashSet<elaborated::Qualified<DataType>>,

    /// Namespaces where things that got imported matter
    import_namespace: Id<id::Namespace>,

    /// The current namespace
    namespace: Id<id::Namespace>,
}

impl<'a> Context<'a> {
    pub fn new(
        reporter: Report,
        file: Id<id::File>,
        namespace: Id<id::Namespace>,
        modules: &'a Modules,
    ) -> Self {
        Self {
            file,
            reporter,
            scopes: Kaleidoscope::default(),
            captured: vec![],
            modules,
            available: ImportMap::new(),
            aliases: HashMap::new(),
            imported: HashSet::new(),
            import_namespace: namespace,
            namespace,
        }
    }

    fn report(&self, message: ResolverErrorKind, range: Range<Byte>) {
        self.reporter.report(Diagnostic::new(ResolverError {
            message,
            range,
            file: self.file,
        }));
    }

    fn scope<T: Scopeable, U>(&mut self, fun: impl FnOnce(&mut Self) -> U) -> U {
        self.scopes.push::<T>();
        let result = fun(self);
        self.scopes.pop::<T>();
        result
    }

    fn add<T: Scopeable>(&mut self, symbol: Symbol) {
        self.scopes.add::<T>(symbol);
    }

    fn capture_scope<U>(&mut self, fun: impl FnOnce(&mut Self) -> U) -> U {
        let not_capturing = self.captured.is_empty();

        if not_capturing {
            self.captured.push(HashMap::new());
        }

        let result = fun(self);

        if not_capturing {
            for name in self.captured.pop().unwrap().keys() {
                self.scopes.add::<Variable>(name.clone());
            }
        }

        result
    }

    /// Gets a path and returns it's canonical version using the `use` statements that are aliases.
    /// It also appends the rest of the path to the canonical path if the first segment of the path
    /// is a alias.
    fn canonicalize(
        &mut self,
        range: Range<Byte>,
        path: Vec<Symbol>,
    ) -> Result<Option<Id<id::Namespace>>, ()> {
        if !path.is_empty() {
            let mut rest = &path[..];

            let sub_tree = if let Some(fst) = self.aliases.get(&path[0]) {
                if let Some(res) = self.modules.tree.find_sub_tree(fst) {
                    rest = &path[1..];
                    res
                } else {
                    &self.modules.tree
                }
            } else {
                &self.modules.tree
            };

            if let Some(res) = sub_tree.find(rest) {
                Ok(Some(res))
            } else {
                self.report(ResolverErrorKind::CannotFindModule(path), range);
                Err(())
            }
        } else {
            Ok(None)
        }
    }

    fn find_import(
        &self,
        range: Range<Byte>,
        name: &DataType,
        report: bool,
    ) -> Result<resolved::Qualified, ()> {
        if let Some(res) = self.available.get(name) {
            if res.is_ambiguous() {
                self.report(ResolverErrorKind::AmbiguousImport(name.clone()), range);
                return Err(());
            }

            let qualified = res.get_canonical();

            let resolved::Qualified {
                canonical, last, ..
            } = qualified.clone();

            if !self.modules.definitions[canonical.0].decls.contains(name) {
                if report {
                    self.report(ResolverErrorKind::CannotFind(name.clone()), range);
                }
                return Err(());
            }

            Ok(resolved::Qualified {
                canonical,
                last,
                range,
            })
        } else if self.modules.definitions[self.import_namespace.0]
            .decls
            .contains(name)
        {
            Ok(resolved::Qualified {
                canonical: self.import_namespace,
                last: name.symbol().clone(),
                range,
            })
        } else {
            if report {
                self.report(ResolverErrorKind::CannotFind(name.clone()), range);
            }
            Err(())
        }
    }

    fn find_on_namespace(
        &self,
        range: Range<Byte>,
        name: &DataType,
        namespace: Id<id::Namespace>,
    ) -> Result<resolved::Qualified, ()> {
        if self.modules.definitions[namespace.0].decls.contains(name) {
            Ok(resolved::Qualified {
                canonical: namespace,
                last: name.symbol().clone(),
                range,
            })
        } else {
            Err(())
        }
    }

    fn find(
        &mut self,
        qualified: &Qualified,
        report: bool,
        typ: fn(Symbol) -> DataType,
    ) -> Result<resolved::Qualified, ()> {
        let canonical = self.canonicalize(qualified.range.clone(), qualified.to_path());
        let data = typ(qualified.last.0.clone());

        let result = if let Err(()) = canonical {
            Err(())
        } else if let Ok(Some(id)) = canonical {
            self.find_on_namespace(qualified.last.1.clone(), &data, id)
        } else {
            self.find_import(qualified.last.1.clone(), &data, report)
        };

        if let Ok(res) = &result {
            self.imported.insert(elaborated::Qualified {
                canonical: res.canonical,
                last: typ(res.last.clone()),
                range: res.range.clone(),
            });
        }

        result
    }
}

pub trait Resolve {
    type Out;

    fn resolve(self, context: &mut Context) -> Self::Out;
}

impl<T: Resolve> Resolve for Spanned<T> {
    type Out = Spanned<T::Out>;

    fn resolve(self, context: &mut Context) -> Self::Out {
        Spanned {
            data: self.data.resolve(context),
            range: self.range,
        }
    }
}

impl<T: Resolve> Resolve for Option<T> {
    type Out = Option<T::Out>;

    fn resolve(self, context: &mut Context) -> Self::Out {
        self.map(|t| t.resolve(context))
    }
}

impl Resolve for Ident {
    type Out = resolved::Ident;

    fn resolve(self, _: &mut Context) -> Self::Out {
        Spanned {
            data: self.0,
            range: self.1,
        }
    }
}

impl Resolve for Effects {
    type Out = resolved::Effects;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::Effects {
            effects: self
                .effects
                .into_iter()
                .map(|e| e.resolve(context))
                .collect(),
        }
    }
}

impl Resolve for TypeArrow {
    type Out = resolved::TypeArrow;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::TypeArrow {
            left: Box::new(self.left.resolve(context)),
            effects: self.effects.resolve(context),
            right: Box::new(self.right.resolve(context)),
        }
    }
}

impl Resolve for TypeApplication {
    type Out = resolved::TypeApplication;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::TypeApplication {
            fun: Box::new(self.left.resolve(context)),
            args: self.right.into_iter().map(|t| t.resolve(context)).collect(),
        }
    }
}

impl Resolve for TypeForall {
    type Out = resolved::TypeForall;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::TypeForall {
            params: self
                .params
                .into_iter()
                .map(|p| p.resolve(context))
                .collect(),
            body: Box::new(self.body.resolve(context)),
        }
    }
}

impl Resolve for TypeKind {
    type Out = resolved::TypeKind;

    fn resolve(self, context: &mut Context) -> Self::Out {
        match self {
            TypeKind::Upper(u) => {
                if let Ok(res) = context.find(&u, true, DataType::Type) {
                    resolved::TypeKind::Upper(res)
                } else {
                    resolved::TypeKind::Error
                }
            }
            TypeKind::Lower(l) => resolved::TypeKind::Lower(l.resolve(context)),
            TypeKind::Arrow(a) => resolved::TypeKind::Arrow(a.resolve(context)),
            TypeKind::Application(a) => resolved::TypeKind::Application(a.resolve(context)),
            TypeKind::Forall(f) => resolved::TypeKind::Forall(f.resolve(context)),
            TypeKind::Unit => resolved::TypeKind::Unit,
        }
    }
}

impl Resolve for Literal {
    type Out = resolved::Literal;

    fn resolve(self, context: &mut Context) -> Self::Out {
        let response = match self.data {
            LiteralKind::String(d) => context
                .find_import(d.1.clone(), &DataType::Type(Symbol::intern("String")), true)
                .map(|x| resolved::LiteralKind::String(d.resolve(context), x))
                .unwrap_or(resolved::LiteralKind::Error),
            LiteralKind::Integer(d) => context
                .find_import(d.1.clone(), &DataType::Type(Symbol::intern("Int")), true)
                .map(|x| resolved::LiteralKind::Integer(d.resolve(context), x))
                .unwrap_or(resolved::LiteralKind::Error),
            LiteralKind::Char(d) => context
                .find_import(d.1.clone(), &DataType::Type(Symbol::intern("Char")), true)
                .map(|x| resolved::LiteralKind::Char(d.resolve(context), x))
                .unwrap_or(resolved::LiteralKind::Error),
            LiteralKind::Float(d) => context
                .find_import(d.1.clone(), &DataType::Type(Symbol::intern("Float")), true)
                .map(|x| resolved::LiteralKind::Float(d.resolve(context), x))
                .unwrap_or(resolved::LiteralKind::Error),
            LiteralKind::Unit => resolved::LiteralKind::Unit,
        };

        Spanned {
            data: response,
            range: self.range,
        }
    }
}

impl Resolve for PatAnnotation {
    type Out = resolved::PatAnnotation;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::PatAnnotation {
            pat: Box::new(self.pat.resolve(context)),
            ty: Box::new(self.ty.resolve(context)),
        }
    }
}

impl Resolve for PatOr {
    type Out = resolved::PatternKind;

    fn resolve(self, context: &mut Context) -> Self::Out {
        context.captured.push(HashMap::new());
        let left_op = self.left.resolve(context);
        let left = context.captured.pop().unwrap();
        context.captured.push(HashMap::new());
        let right_op = self.right.resolve(context);
        let right = context.captured.pop().unwrap();

        let leftcol = left.keys().cloned().collect::<HashSet<_>>();
        let rightcol = right.keys().cloned().collect::<HashSet<_>>();

        let diff = rightcol.symmetric_difference(&leftcol);

        let mut errored = false;

        for key in diff {
            errored = true;

            let range = right
                .get(key)
                .unwrap_or_else(|| left.get(key).unwrap())
                .clone();

            context.report(
                ResolverErrorKind::VariableNotBoundOnBothSides(key.clone()),
                range.clone(),
            );
        }

        context.captured.last_mut().unwrap().extend(left);

        if errored {
            resolved::PatternKind::Error
        } else {
            resolved::PatternKind::Or(resolved::PatOr {
                left: Box::new(left_op),
                right: Box::new(right_op),
            })
        }
    }
}

impl Resolve for PatternKind {
    type Out = resolved::PatternKind;

    fn resolve(self, context: &mut Context) -> Self::Out {
        context.capture_scope(|context| match self {
            PatternKind::Upper(qualified) => {
                let canonical = context.find(&qualified, true, DataType::Constructor);
                if let Ok(canonical) = canonical {
                    resolved::PatternKind::Application(resolved::PatApplication {
                        func: canonical,
                        args: vec![],
                    })
                } else {
                    resolved::PatternKind::Error
                }
            }
            PatternKind::Lower(l) => {
                if context.captured.iter().any(|c| c.contains_key(&l.0)) {
                    context.report(
                        ResolverErrorKind::VariableAlreadyCaptured(l.0.clone()),
                        l.1.clone(),
                    );
                } else {
                    context
                        .captured
                        .last_mut()
                        .unwrap()
                        .insert(l.0.clone(), l.1.clone());
                }
                resolved::PatternKind::Lower(l.resolve(context))
            }
            PatternKind::Or(or) => or.resolve(context),
            PatternKind::Wildcard => resolved::PatternKind::Wildcard,
            PatternKind::Literal(l) => resolved::PatternKind::Literal(l.resolve(context)),
            PatternKind::Annotation(ann) => resolved::PatternKind::Annotation(ann.resolve(context)),
            PatternKind::Application(app) => {
                let args = app.args.into_iter().map(|a| a.resolve(context)).collect();
                let cons = context.find(&app.func, true, DataType::Constructor);

                if let Ok(cons) = cons {
                    resolved::PatternKind::Application(resolved::PatApplication {
                        func: cons,
                        args,
                    })
                } else {
                    resolved::PatternKind::Error
                }
            }
        })
    }
}

impl<T: Resolve> Resolve for Vec<T> {
    type Out = Vec<T::Out>;

    fn resolve(self, context: &mut Context) -> Self::Out {
        self.into_iter().map(|t| t.resolve(context)).collect()
    }
}

impl Resolve for LambdaExpr {
    type Out = resolved::LambdaExpr;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::LambdaExpr {
            pattern: self.pattern.resolve(context),
            body: Box::new(self.body.resolve(context)),
        }
    }
}

impl Resolve for ApplicationExpr {
    type Out = resolved::ApplicationExpr;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::ApplicationExpr {
            func: Box::new(self.func.resolve(context)),
            args: self.args.resolve(context),
        }
    }
}

impl Resolve for AcessorExpr {
    type Out = resolved::AcessorExpr;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::AcessorExpr {
            expr: Box::new(self.expr.resolve(context)),
            field: self.field.resolve(context),
        }
    }
}

impl Resolve for LetExpr {
    type Out = resolved::LetExpr;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::LetExpr {
            name: Box::new(self.name.resolve(context)),
            value: Box::new(self.value.resolve(context)),
            body: Box::new(self.body.resolve(context)),
        }
    }
}

impl Resolve for Spanned<Operator> {
    type Out = resolved::Expr;

    fn resolve(self, ctx: &mut Context) -> Self::Out {
        let name = match self.data {
            Operator::Add => "add",
            Operator::Sub => "sub",
            Operator::Mul => "mul",
            Operator::Div => "div",
            Operator::Rem => "rem",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::Xor => "xor",
            Operator::Not => "not",
            Operator::Eq => "eq",
            Operator::Neq => "neq",
            Operator::Lt => "lt",
            Operator::Gt => "gt",
            Operator::Le => "le",
            Operator::Ge => "ge",
            Operator::Shl => "shl",
            Operator::Shr => "shr",
            Operator::Pipe => "pipe",
        };

        let data = ctx
            .find_import(
                self.range.clone(),
                &DataType::Type(Symbol::intern(name)),
                true,
            )
            .map(resolved::ExprKind::Function)
            .unwrap_or(resolved::ExprKind::Error);

        Spanned {
            data,
            range: self.range,
        }
    }
}

impl Resolve for WhenArm {
    type Out = resolved::WhenArm;

    fn resolve(self, context: &mut Context) -> Self::Out {
        context.scope::<scopable::Variable, _>(|context| resolved::WhenArm {
            pattern: Box::new(self.pattern.resolve(context)),
            then: Box::new(self.then.resolve(context)),
        })
    }
}

impl Resolve for WhenExpr {
    type Out = resolved::WhenExpr;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::WhenExpr {
            scrutinee: Box::new(self.scrutinee.resolve(context)),
            arms: self.arms.resolve(context),
        }
    }
}

impl Resolve for AnnotationExpr {
    type Out = resolved::AnnotationExpr;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::AnnotationExpr {
            expr: Box::new(self.expr.resolve(context)),
            ty: Box::new(self.ty.resolve(context)),
        }
    }
}

impl Resolve for LetStmt {
    type Out = resolved::LetStmt;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::LetStmt {
            name: Box::new(self.name.resolve(context)),
            expr: Box::new(self.expr.resolve(context)),
        }
    }
}

impl Resolve for StatementKind {
    type Out = resolved::StatementKind;

    fn resolve(self, context: &mut Context) -> Self::Out {
        match self {
            StatementKind::Let(let_) => resolved::StatementKind::Let(let_.resolve(context)),
            StatementKind::Expr(expr) => resolved::StatementKind::Expr(expr.resolve(context)),
            StatementKind::Error => resolved::StatementKind::Error,
        }
    }
}

impl Resolve for Block {
    type Out = resolved::Block;

    fn resolve(self, context: &mut Context) -> Self::Out {
        context.scope::<scopable::Variable, _>(|context| resolved::Block {
            statements: self.statements.resolve(context),
        })
    }
}

impl Resolve for ExprKind {
    type Out = resolved::ExprKind;

    fn resolve(self, context: &mut Context) -> Self::Out {
        match self {
            ExprKind::Ident(qualified) => {
                if context.scopes.contains::<Variable>(&qualified.last.0)
                    && qualified.segments.is_empty()
                {
                    return resolved::ExprKind::Variable(qualified.last.clone().into());
                }

                if let Ok(canonical) = context.find(&qualified, false, DataType::Let) {
                    resolved::ExprKind::Function(canonical)
                } else if let Ok(canonical) = context.find(&qualified, true, DataType::Constructor)
                {
                    resolved::ExprKind::Constructor(canonical)
                } else {
                    resolved::ExprKind::Error
                }
            }
            ExprKind::Lambda(lambda) => resolved::ExprKind::Lambda(lambda.resolve(context)),
            ExprKind::Application(app) => resolved::ExprKind::Application(app.resolve(context)),
            ExprKind::Acessor(acc) => resolved::ExprKind::Acessor(acc.resolve(context)),
            ExprKind::Let(let_) => resolved::ExprKind::Let(let_.resolve(context)),
            ExprKind::When(when) => resolved::ExprKind::When(when.resolve(context)),
            ExprKind::Annotation(ann) => resolved::ExprKind::Annotation(ann.resolve(context)),
            ExprKind::Block(block) => resolved::ExprKind::Block(block.resolve(context)),
            ExprKind::Literal(lit) => resolved::ExprKind::Literal(lit.resolve(context)),
            ExprKind::Binary(_) => todo!(),
            ExprKind::If(_) => todo!(),
        }
    }
}

impl Resolve for Field {
    type Out = resolved::Field;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::Field {
            name: self.name.resolve(context),
            ty: Box::new(self.ty.resolve(context)),
        }
    }
}

impl Resolve for Variant {
    type Out = resolved::Variant;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::Variant {
            name: self.name.resolve(context),
            args: self.args.resolve(context),
        }
    }
}

impl Resolve for EnumDecl {
    type Out = resolved::EnumDecl;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::EnumDecl {
            variants: self.variants.resolve(context),
        }
    }
}

impl Resolve for RecordDecl {
    type Out = resolved::RecordDecl;

    fn resolve(self, context: &mut Context) -> Self::Out {
        resolved::RecordDecl {
            fields: self.fields.resolve(context),
        }
    }
}

impl Resolve for TypeDef {
    type Out = resolved::TypeDef;

    fn resolve(self, context: &mut Context) -> Self::Out {
        match self {
            TypeDef::Enum(enum_) => resolved::TypeDef::Enum(enum_.resolve(context)),
            TypeDef::Record(record) => resolved::TypeDef::Record(record.resolve(context)),
            TypeDef::Synonym(syn) => resolved::TypeDef::Synonym(syn.resolve(context)),
        }
    }
}

impl Resolve for TypeDecl {
    type Out = resolved::TypeDecl;

    fn resolve(self, context: &mut Context) -> Self::Out {
        let ns = context.namespace;
        context.namespace = self.id.unwrap();

        let result = context.scope::<TypeVariable, _>(|context| {
            let params = self.params.resolve(context);

            for param in &params {
                context.add::<TypeVariable>(param.data.clone())
            }

            resolved::TypeDecl {
                id: self.id.unwrap(),
                name: self.name.resolve(context),
                params,
                def: self.def.resolve(context),
            }
        });

        context.namespace = ns;
        result
    }
}

impl<T: Resolve, U: Resolve> Resolve for (T, U) {
    type Out = (T::Out, U::Out);

    fn resolve(self, context: &mut Context) -> Self::Out {
        (self.0.resolve(context), self.1.resolve(context))
    }
}

impl Resolve for LetCase {
    type Out = resolved::LetCase;

    fn resolve(self, context: &mut Context) -> Self::Out {
        context.scope::<scopable::Variable, _>(|context| resolved::LetCase {
            patterns: context.capture_scope(|context| self.patterns.resolve(context)),
            body: Box::new(self.body.resolve(context)),
            range: self.range,
        })
    }
}

impl Resolve for LetDecl {
    type Out = resolved::LetDecl;

    fn resolve(self, context: &mut Context) -> Self::Out {
        context.scope::<Variable, _>(|context| resolved::LetDecl {
            name: self.name.resolve(context),
            params: self.params.resolve(context),
            cases: self.cases.resolve(context),
            ret: self.ret.resolve(context),
        })
    }
}

impl Resolve for UseDecl {
    type Out = ();

    fn resolve(self, context: &mut Context) -> Self::Out {
        let path = self.path.to_entire_path();
        let Some(id) = context.modules.find_module(&path) else {
            context.report(ResolverErrorKind::CannotFindModule(path), self.path.range);
            return;
        };
        if let Some(alias) = self.alias {
            context.aliases.insert(alias.0, path);
        } else {
            let definitions = &context.modules.definitions[id.0];

            for defs in &definitions.decls {
                context.available.add(
                    defs.clone(),
                    resolved::Qualified {
                        canonical: id,
                        last: defs.symbol().clone(),
                        range: self.path.range.clone(),
                    },
                )
            }
        }
    }
}

impl Resolve for Program {
    type Out = resolved::Program;

    fn resolve(self, context: &mut Context) -> Self::Out {
        self.uses.resolve(context);

        resolved::Program {
            id: context.namespace,
            types: self.types.resolve(context),
            lets: self.lets.resolve(context),
        }
    }
}
