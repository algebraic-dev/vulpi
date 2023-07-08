use std::ops::Range;
use std::path::PathBuf;

use crate::error::{ResolverError, ResolverErrorKind};
use crate::scope::{self, Kaleidoscope, Scopeable};
use crate::Resolver;

use vulpi_desugar::desugar;
use vulpi_location::{Byte, Location};
use vulpi_parser::parse;
use vulpi_report::{Diagnostic, IntoDiagnostic, Report};
use vulpi_storage::id::{self, Id};
use vulpi_storage::namespace::{self, Name, Namespace, Namespaces, Qualified};
use vulpi_storage::vfs::{self, FileSystem};
use vulpi_syntax::r#abstract;

pub fn to_namespace_path(path: &r#abstract::Qualified) -> namespace::Path {
    namespace::Path(
        path.segments
            .iter()
            .map(|x| x.0.clone())
            .chain(std::iter::once(path.last.0.clone()))
            .collect::<Vec<_>>(),
    )
}

pub enum Action {
    Cached(Id<id::Namespace>),
    Created(Id<id::Namespace>),
    Failed,
}

pub struct Context {
    pub file_system: Box<dyn FileSystem<PathBuf, String>>,
    pub scopes: Kaleidoscope,
    reporter: Report,
    namespaces: Namespaces<()>,
    actual: Vec<Id<id::Namespace>>,
}

impl Context {
    pub fn new(file_system: impl FileSystem<PathBuf, String> + 'static, reporter: Report) -> Self {
        Self {
            file_system: Box::new(file_system),
            scopes: Kaleidoscope::default(),
            reporter,
            namespaces: Namespaces::default(),
            actual: Vec::new(),
        }
    }

    pub fn load(&mut self, range: Range<Byte>, path: namespace::Path) -> Action {
        if let Some(id) = self.namespaces.get_id(&path) {
            return Action::Cached(id);
        }

        self.create(range, path)
    }

    pub fn report(&mut self, diagnostic: impl IntoDiagnostic + 'static) {
        self.reporter.report(Diagnostic::new(diagnostic));
    }

    pub fn enter(&mut self, id: Id<id::Namespace>) {
        self.actual.push(id);
    }

    pub fn create(&mut self, range: Range<Byte>, path: namespace::Path) -> Action {
        let real_path = path.to_path("vp");

        match self.create_from_file(path.clone(), real_path) {
            Ok(id) => Action::Created(id),
            Err(_) => {
                let file = self.actual().file_id;
                self.reporter.report(Diagnostic::new(ResolverError {
                    location: Location { file, range },
                    kind: ResolverErrorKind::CantFindModule(path),
                }));
                Action::Failed
            }
        }
    }

    pub fn create_from_file(
        &mut self,
        path: namespace::Path,
        file: PathBuf,
    ) -> Result<Id<id::Namespace>, vfs::Error> {
        let file_id = self.file_system.load(file)?;
        let str = self.file_system.read(file_id)?;

        let namespace = Namespace::new(path.clone(), file_id);
        let id = self.namespaces.add(path, namespace);

        self.actual.push(id);

        let lexer = vulpi_parser::Lexer::new(str);

        let program = parse(lexer, file_id, self.reporter.clone());
        let mut desugared = desugar(program, self.reporter.clone(), file_id);

        desugared.id = Some(id);

        desugared.declare(self);
        desugared.resolve(self);

        self.unload();

        Ok(id)
    }

    pub fn get(&self, id: Id<id::Namespace>) -> &Namespace<()> {
        self.namespaces.get(id).unwrap()
    }

    pub fn get_by_path(&self, path: &namespace::Path) -> Option<&Namespace<()>> {
        self.namespaces.get_by_path(path)
    }

    /// TODO: Handle duplicated alias
    pub fn alias(&mut self, path: namespace::Path, alias: namespace::Path) {
        let namespace = self.actual();
        namespace.aliases.insert(alias, path);
    }

    /// TODO: Error handling when duplicated
    pub fn declare_namespace(
        &mut self,
        path: namespace::Path,
        namespace: Namespace<()>,
    ) -> Id<id::Namespace> {
        let id = self.namespaces.add(path, namespace);

        self.actual.push(id);

        id
    }

    pub fn declare_type(&mut self, name: Name) {
        self.actual().declare_type(name);
    }

    pub fn declare_value(&mut self, name: Name) {
        self.actual().declare_value(name);
    }

    pub fn add_used_type(&mut self, name: Qualified) {
        self.actual().add_used_type(name);
    }

    pub fn add_used_value(&mut self, name: Qualified) {
        self.actual().add_used_value(name);
    }

    pub fn unload(&mut self) -> Id<id::Namespace> {
        self.actual.pop().unwrap()
    }

    pub fn get_actual(&mut self) -> Option<&mut Namespace<()>> {
        self.actual.last().and_then(|x| self.namespaces.get_mut(*x))
    }

    pub fn actual(&mut self) -> &mut Namespace<()> {
        self.get_actual().unwrap()
    }

    pub fn scope<T: Scopeable, U>(&mut self, fun: impl FnOnce(&mut Self) -> U) -> U {
        self.scopes.push::<T>();
        let result = fun(self);
        self.scopes.pop::<T>();
        result
    }
}
