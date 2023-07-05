pub mod error;
pub mod scope;

use error::Result;

use scope::Kaleidoscope;

use vulpi_report::{IntoDiagnostic, Reporter};
use vulpi_storage::namespace::{Name, Namespace, Namespaces, Path};

pub type Loader = dyn FnMut(Path) -> Result<()>;

/// The resolver context. It store scopes and other things that are needed in order to resolve the
/// symbols.
pub struct Context<'a> {
    scope: &'a mut Kaleidoscope,
    reporter: &'a mut dyn Reporter,
    load: &'a mut Loader,

    actual_namespace: Namespace<()>,
    namespaces: &'a mut Namespaces<()>,
}

impl<'a> Context<'a> {
    pub fn new(
        scope: &'a mut Kaleidoscope,
        reporter: &'a mut dyn Reporter,
        load: &'a mut Loader,
        namespaces: &'a mut Namespaces<()>,
        path: Path,
    ) -> Self {
        Self {
            scope,
            reporter,
            load,
            actual_namespace: Namespace::new(path),
            namespaces,
        }
    }

    pub fn scope<T: scope::Scopeable>(&mut self, fun: impl FnOnce(&mut Self)) {
        T::scope_mut(self.scope).push();
        fun(self);
        T::scope_mut(self.scope).pop();
    }

    pub fn report(&mut self, error: impl IntoDiagnostic + 'static) {
        self.reporter.report(Box::new(error));
    }

    pub fn load(&mut self, path: Path) -> Result<()> {
        (self.load)(path)
    }

    pub fn define(&mut self, name: Name, value: ()) {
        self.actual_namespace.define(name, value);
    }

    pub fn declare(&mut self, name: Name) {
        self.actual_namespace.declare(name);
    }
}

pub trait Resolvable<'a> {
    fn declare(&'a mut self, ctx: &mut Context);
}
