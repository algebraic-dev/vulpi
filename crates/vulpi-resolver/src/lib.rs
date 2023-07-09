//! The name resolution module. It creates a interface for each of the namespaces and modules and
//! then resolves the names in the source code to the correct interface.

pub mod error;
pub mod scope;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    ops::Range,
    path::PathBuf,
};

use error::{ResolverError, ResolverErrorKind};
use scope::scopable::{PatternVariable, TypeVariable};
use scope::{Kaleidoscope, Scopeable};

use vulpi_desugar::desugar;
use vulpi_location::{Byte, Location, Spanned};
use vulpi_parser::{parse, Lexer};
use vulpi_report::{Diagnostic, IntoDiagnostic, Report};
use vulpi_storage::file_system::FileSystem;
use vulpi_storage::id::{self, Id};
use vulpi_storage::{file_system, interner::Symbol};
use vulpi_syntax::r#abstract;
use vulpi_syntax::r#abstract::*;

/// A name is a symbol that is used to refer to a variable, function, type, etc.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(Symbol);

impl From<Ident> for Name {
    fn from(ident: Ident) -> Self {
        Self(ident.0)
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.get())
    }
}

/// A path is a list of names that are used to refer to a module.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path(Vec<Symbol>);

impl Path {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, name: Symbol) {
        self.0.push(name);
    }

    pub fn pop(&mut self) -> Option<Symbol> {
        self.0.pop()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn to_path_buf(&self) -> std::path::PathBuf {
        let mut path_buf = std::path::PathBuf::new();

        for name in self.0.iter() {
            path_buf.push(name.get());
        }

        path_buf.set_extension("vp");

        path_buf
    }
}

impl From<r#abstract::Qualified> for Path {
    fn from(qualified: r#abstract::Qualified) -> Self {
        Path(
            qualified
                .segments
                .iter()
                .map(|s| s.0.clone())
                .chain(std::iter::once(qualified.last.0.clone()))
                .collect::<Vec<_>>(),
        )
    }
}

impl From<&str> for Path {
    fn from(path: &str) -> Self {
        Path(path.split('.').map(Symbol::intern).collect::<Vec<_>>())
    }
}

impl Default for Path {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0
            .iter()
            .map(|x| x.get())
            .collect::<Vec<_>>()
            .join(".")
            .fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qualified {
    pub path: Path,
    pub name: Name,
}

impl Qualified {
    pub fn new(path: Path, name: Name) -> Self {
        Self { path, name }
    }
}

impl From<r#abstract::Qualified> for Qualified {
    fn from(qualified: r#abstract::Qualified) -> Self {
        let path = Path(
            qualified
                .segments
                .iter()
                .map(|s| s.0.clone())
                .collect::<Vec<_>>(),
        );
        Self {
            name: qualified.last.into(),
            path,
        }
    }
}

impl Display for Qualified {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.path.is_empty() {
            self.name.fmt(f)
        } else {
            write!(f, "{}.{}", self.path, self.name)
        }
    }
}

#[derive(Clone, Debug)]
pub enum Ambiguity<K> {
    Single(K),
    Ambiguious(HashSet<K>),
}

impl<K: PartialEq + Eq + Hash + Clone> Ambiguity<K> {
    pub fn new(name: K) -> Self {
        Self::Single(name)
    }

    pub fn add(&mut self, name: K) {
        match self {
            Self::Single(single) if *single != name => {
                let mut set = HashSet::new();
                set.insert(single.clone());
                set.insert(name);
                *self = Self::Ambiguious(set);
            }
            Self::Ambiguious(set) => {
                set.insert(name);
            }
            _ => (),
        }
    }
}

#[derive(Clone, Debug)]
pub struct OccurenceMap<K, V> {
    pub data: HashMap<K, Ambiguity<V>>,
}

impl<K, V> Default for OccurenceMap<K, V> {
    fn default() -> Self {
        Self {
            data: Default::default(),
        }
    }
}

impl<K: Eq + Hash, V: PartialEq + Eq + Hash + Clone> OccurenceMap<K, V> {
    pub fn insert(&mut self, key: K, value: V) {
        match self.data.get_mut(&key) {
            Some(ambiguity) => ambiguity.add(value),
            None => {
                self.data.insert(key, Ambiguity::new(value));
            }
        }
    }

    pub fn get(&self, key: &K) -> Option<&Ambiguity<V>> {
        self.data.get(key)
    }
}

#[derive(Debug, Clone)]

pub enum Visibility {
    Public,
    Private,
}

#[derive(Default, Debug, Clone)]
pub struct Leveled<T> {
    pub types: T,
    pub values: T,
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub path: Path,
    pub declared: Leveled<HashMap<Name, Visibility>>,
    pub constructors: HashMap<Name, Visibility>,
    pub fields: OccurenceMap<Name, Qualified>,
}

impl Interface {
    pub fn new(path: Path) -> Self {
        Self {
            path,
            declared: Default::default(),
            fields: Default::default(),
            constructors: Default::default(),
        }
    }

    pub fn add_value(&mut self, name: Name, visibility: Visibility) {
        self.declared.values.insert(name, visibility);
    }

    pub fn add_type(&mut self, name: Name, visibility: Visibility) {
        self.declared.types.insert(name, visibility);
    }

    pub fn add_constructor(&mut self, name: Name, visibility: Visibility) {
        self.constructors.insert(name, visibility);
    }

    pub fn add_field(&mut self, field: Name, typ: Qualified) {
        self.fields.insert(field, typ);
    }
}

#[derive(Default, Debug, Clone)]
pub struct Imports {
    pub aliases: OccurenceMap<Name, Path>,
    pub uses: Leveled<OccurenceMap<Name, Qualified>>,
    pub fields: OccurenceMap<Name, Qualified>,
}

impl Imports {
    pub fn alias(&mut self, from: Name, to: Path) {
        self.aliases.insert(from, to);
    }

    pub fn use_type(&mut self, name: Name, path: Qualified) {
        self.uses.types.insert(name, path);
    }

    pub fn use_value(&mut self, name: Name, path: Qualified) {
        self.uses.values.insert(name, path);
    }

    pub fn import(&mut self, interface: &Interface) {
        // TODO: add visibility
        for name in interface.declared.types.keys() {
            self.use_type(
                name.clone(),
                Qualified::new(interface.path.clone(), name.clone()),
            )
        }

        for name in interface.declared.values.keys() {
            self.use_value(
                name.clone(),
                Qualified::new(interface.path.clone(), name.clone()),
            )
        }
    }
}

pub struct Module {
    pub file: Id<id::File>,
    pub id: Id<id::Namespace>,
    pub program: Program,
    pub interface: Interface,
    pub imports: Imports,
}

impl Module {
    pub fn new(
        file: Id<id::File>,
        id: Id<id::Namespace>,
        program: Program,
        interface: Interface,
        dependencies: Imports,
    ) -> Self {
        Self {
            file,
            id,
            program,
            interface,
            imports: dependencies,
        }
    }
}

#[derive(Default)]
pub struct ModuleTree {
    id: Option<Id<id::Namespace>>,
    children: HashMap<Symbol, ModuleTree>,
}

impl ModuleTree {
    pub fn add(&mut self, path: Path, id: Id<id::Namespace>) {
        fn add(this: &mut ModuleTree, path: &[Symbol], id: Id<id::Namespace>) {
            if path.is_empty() {
                this.id = Some(id);
            } else {
                let name = path[0].clone();

                let child = this
                    .children
                    .entry(name)
                    .or_insert_with(ModuleTree::default);

                add(child, &path[1..], id);
            }
        }

        add(self, &path.0, id);
    }

    pub fn find(&self, path: Path) -> Option<&ModuleTree> {
        pub fn find<'a>(this: &'a ModuleTree, path: &[Symbol]) -> Option<&'a ModuleTree> {
            if path.is_empty() {
                Some(this)
            } else {
                let name = &path[0];
                this.children
                    .get(name)
                    .and_then(|child| find(child, &path[1..]))
            }
        }

        find(self, &path.0)
    }
}

pub struct Context<'a> {
    pub scopes: Kaleidoscope,

    pub file: Id<id::File>,
    pub reporter: Report,
    pub imports: Imports,
    pub interface: Interface,

    pub file_system: &'a mut dyn FileSystem<PathBuf, String>,
    pub modules: &'a mut Vec<Module>,
    pub interfaces: &'a mut Vec<Interface>,
    pub map: &'a mut HashMap<Path, Id<id::Namespace>>,
    pub tree: &'a mut ModuleTree,
}

pub enum Kind {
    Type,
    Constructor,
    Value,
}

impl<'a> Context<'a> {
    pub fn new(
        reporter: Report,
        path: Path,
        file_system: &'a mut dyn FileSystem<PathBuf, String>,
        modules: &'a mut Vec<Module>,
        interfaces: &'a mut Vec<Interface>,
        map: &'a mut HashMap<Path, Id<id::Namespace>>,
        tree: &'a mut ModuleTree,
    ) -> Self {
        Self {
            scopes: Default::default(),
            file: Default::default(),
            reporter,
            imports: Default::default(),
            interface: Interface::new(path),
            map,
            file_system,
            modules,
            interfaces,
            tree,
        }
    }

    pub fn report(&mut self, message: impl IntoDiagnostic + 'static) {
        self.reporter.report(Diagnostic::new(message));
    }

    pub fn scope<T: Scopeable, U>(&mut self, fun: impl FnOnce(&mut Context) -> U) -> U {
        self.scopes.push::<T>();
        let result = fun(self);
        self.scopes.pop::<T>();
        result
    }

    pub fn add<T: Scopeable>(&mut self, name: Name) {
        self.scopes.add::<T>(name);
    }

    pub fn contains<T: Scopeable>(&self, name: &Name) -> bool {
        self.scopes.contains::<T>(name)
    }

    pub fn load(&mut self, place: Range<Byte>, path: Path) -> Option<Id<id::Namespace>> {
        if let Some(id) = self.map.get(&path) {
            return Some(*id);
        }

        let file = path.to_path_buf();

        match self.load_from_file(path.clone(), file) {
            Ok(id) => Some(id),
            // TODO: Better error here
            Err(_) => {
                self.report(ResolverError {
                    location: Location::new(self.file, place),
                    kind: error::ResolverErrorKind::CantFindModule(path),
                });

                None
            }
        }
    }

    fn derive(&mut self, path: Path) -> Context<'_> {
        Context {
            imports: Default::default(),
            scopes: Kaleidoscope::default(),
            interface: Interface::new(path),

            reporter: self.reporter.clone(),
            file_system: self.file_system,
            modules: self.modules,
            file: self.file,
            map: self.map,
            interfaces: self.interfaces,
            tree: self.tree,
        }
    }

    pub fn load_from_file(
        &mut self,
        path: Path,
        file: PathBuf,
    ) -> Result<Id<id::Namespace>, file_system::Error> {
        let file = self.file_system.load(file)?;
        let source = self.file_system.read(file)?;

        let lexer = Lexer::new(source);
        let parsed = parse(lexer, file, self.reporter.clone());
        let mut desugared = desugar(parsed, file, self.reporter.clone());

        let mut context = self.derive(path.clone());

        let id = context.interfaces.len();

        context.map.insert(path.clone(), Id::new(id));
        context.tree.add(path, Id::new(id));

        desugared.declare(&mut context);

        desugared.resolve(&mut context);

        let module = Module::new(
            file,
            Id::new(id),
            desugared,
            context.interface,
            context.imports,
        );

        context.modules.push(module);

        Ok(Id::new(id))
    }

    pub fn ambiguity(&mut self, place: Range<Byte>, ambiguity: Ambiguity<Path>) -> Option<Path> {
        match ambiguity {
            Ambiguity::Single(s) => Some(s),
            Ambiguity::Ambiguious(s) => {
                self.report(ResolverError {
                    location: Location::new(self.file, place),
                    kind: error::ResolverErrorKind::Ambiguity(s.into_iter().next().unwrap()),
                });
                None
            }
        }
    }

    pub fn canonicalize_path(
        &mut self,
        range: Range<Byte>,
        mut path: Path,
    ) -> Option<Id<id::Namespace>> {
        if !path.is_empty() {
            if let Some(ambiguity) = self.imports.aliases.get(&Name(path.0[0].clone())) {
                let mut alias = self.ambiguity(range.clone(), ambiguity.clone())?;
                alias.0.extend(path.0.into_iter().skip(1));
                path = alias;
            } else if let Some(tree) = self.tree.find(self.interface.path.clone()) {
                if let Some(id) = tree.find(path.clone()).and_then(|x| x.id) {
                    return Some(id);
                }
            }

            if let Some(id) = self.tree.find(path.clone()).and_then(|x| x.id) {
                Some(id)
            } else {
                self.report(ResolverError {
                    location: Location::new(self.file, range),
                    kind: error::ResolverErrorKind::CantFindModule(path),
                });
                None
            }
        } else {
            None
        }
    }

    pub fn canonicalize(
        &mut self,
        range: Range<Byte>,
        qualified: Qualified,
        kind: Kind,
    ) -> Option<Qualified> {
        if !qualified.path.is_empty() {
            let id = self.canonicalize_path(range.clone(), qualified.path.clone())?;
            let interface = self.interfaces.get(id.index())?;

            let result = match kind {
                Kind::Type => &interface.declared.types,
                Kind::Value => &interface.declared.values,
                Kind::Constructor => &interface.constructors,
            };

            if result.get(&qualified.name).is_some() {
                Some(Qualified::new(interface.path.clone(), qualified.name))
            } else {
                self.report(ResolverError {
                    location: Location::new(self.file, range),
                    kind: error::ResolverErrorKind::CantFindValue(qualified),
                });
                None
            }
        } else {
            todo!()
        }
    }
}

pub trait Declare {
    fn declare(&self, context: &mut Context);
}

impl Declare for LetDecl {
    fn declare(&self, context: &mut Context) {
        let name = self.name.0.clone();
        context.interface.add_value(Name(name), Visibility::Public);
    }
}

impl Declare for Variant {
    fn declare(&self, context: &mut Context) {
        let name = self.name.0.clone();
        context
            .interface
            .add_constructor(Name(name), Visibility::Public);
    }
}

impl Declare for TypeDecl {
    fn declare(&self, context: &mut Context) {
        let name = self.name.0.clone();
        let name = Name(name);

        context.interface.add_type(name, Visibility::Public);

        // TODO: Check if it exists and throw an error

        context.scope::<TypeVariable, _>(|context| {
            for param in &self.params {
                context.add::<TypeVariable>(Name(param.0.clone()));
            }

            match &self.def {
                TypeDef::Enum(enum_) => {
                    for variant in &enum_.variants {
                        variant.declare(context);
                    }
                }
                TypeDef::Record(rec_) => {
                    for field in &rec_.fields {
                        let name = Name(field.name.0.clone());

                        let typ = Qualified {
                            path: context.interface.path.clone(),
                            name: Name(self.name.0.clone()),
                        };

                        context.interface.add_field(name, typ);
                    }
                }
                TypeDef::Synonym(_) => (),
            }
        });
    }
}

impl Declare for UseDecl {
    fn declare(&self, context: &mut Context) {
        let to: Path = self.path.clone().into();

        let id = context.load(self.path.range.clone(), to.clone());

        if let Some(alias) = &self.alias {
            let from = alias.clone().into();
            context.imports.alias(from, to);
        }

        if let Some(id) = id {
            let interface = &context.interfaces[id.index()];
            context.imports.import(interface);
        }
    }
}

impl Declare for Program {
    fn declare(&self, context: &mut Context) {
        for item in &self.lets {
            item.declare(context);
        }

        for item in &self.types {
            item.declare(context);
        }

        context.interfaces.push(context.interface.clone());

        for item in &self.uses {
            item.declare(context);
        }
    }
}

trait Resolve {
    type Output;
    fn resolve(&mut self, context: &mut Context) -> Self::Output;
}

impl<T: Resolve> Resolve for Option<T> {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        if let Some(value) = self {
            value.resolve(context);
        }
    }
}

impl<T: Resolve> Resolve for Vec<T> {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        for value in self {
            value.resolve(context);
        }
    }
}

impl<T: Resolve> Resolve for Spanned<T> {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        self.data.resolve(context);
    }
}

impl<T: Resolve, U: Resolve> Resolve for (T, U) {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        self.0.resolve(context);
        self.1.resolve(context);
    }
}

impl Resolve for PatternKind {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        match self {
            PatternKind::Wildcard => (),
            PatternKind::Upper(path) => {
                let qualified = Qualified::from(path.clone());

                let result = context.canonicalize(path.range.clone(), qualified, Kind::Constructor);

                if let Some(qualified) = result {
                    path.segments = qualified.path.0.into_iter().map(Ident::generate).collect()
                }
            }
            PatternKind::Lower(lower) => {
                let name = Name(lower.0.clone());

                if context.contains::<PatternVariable>(&name) {
                    context.report(ResolverError {
                        location: Location::new(context.file, lower.1.clone()),
                        kind: ResolverErrorKind::DuplicatePatternVariable(name.0),
                    });
                } else {
                    context.add::<PatternVariable>(name);
                }
            }
            PatternKind::Literal(_) => (),
            PatternKind::Annotation(ann) => {
                ann.pat.resolve(context);
                ann.ty.resolve(context);
            }
            PatternKind::Or(or) => {
                or.left.resolve(context);
                or.right.resolve(context);
            }
            PatternKind::Application(app) => {}
        }
    }
}

impl Resolve for TypeKind {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        ()
    }
}

impl Resolve for ExprKind {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        ()
    }
}

impl Resolve for LetCase {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        context.scope::<PatternVariable, _>(|context| {
            self.patterns.resolve(context);
        });
        self.body.resolve(context);
    }
}

impl Resolve for LetDecl {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        self.cases.resolve(context)
    }
}

impl Resolve for TypeDecl {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        ()
    }
}

impl Resolve for Program {
    type Output = ();

    fn resolve(&mut self, context: &mut Context) {
        self.lets.resolve(context);
        self.types.resolve(context);
    }
}
