pub mod scopable {
    pub enum Variable {}
    pub enum TypeVariable {}
    pub enum Module {}

    mod sealed {
        pub trait Scopable {}
        impl Scopable for super::Variable {}
        impl Scopable for super::TypeVariable {}
        impl Scopable for super::Module {}
    }

    pub trait Scopable: sealed::Scopable {}
    impl<T: sealed::Scopable> Scopable for T {}
}

pub use scopable::Scopable;
use vulpi_storage::interner::Symbol;

#[derive(Clone)]
pub struct Scope {
    pub map: Vec<im_rc::HashSet<Symbol>>,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            map: vec![Default::default()],
        }
    }
}

pub struct Scopes<'a>(Vec<&'a Scope>);

pub struct ScopesMut<'a>(Vec<&'a mut Scope>);

impl<'a> ScopesMut<'a> {
    pub fn push(&mut self) {
        for scope in self.0.iter_mut() {
            scope.map.push(Default::default());
        }
    }

    pub fn pop(&mut self) {
        for scope in self.0.iter_mut() {
            scope.map.pop();
        }
    }
}

/// Just a fun name for a structure that contains a bunch of scopes. It's used to resolve the
/// symbols.

#[derive(Clone, Default)]
pub struct Kaleidoscope {
    pub variables: Scope,
    pub type_variables: Scope,
}

pub trait Scoped {
    fn push(kaleidoscope: &mut Kaleidoscope);
    fn pop(kaleidoscope: &mut Kaleidoscope);
}

pub trait Scopeable {
    fn scope(kaleidoscope: &Kaleidoscope) -> Scopes<'_>;
    fn scope_mut(kaleidoscope: &mut Kaleidoscope) -> ScopesMut<'_>;
}

impl Scopeable for scopable::Variable {
    fn scope(kaleidoscope: &Kaleidoscope) -> Scopes<'_> {
        Scopes(vec![&kaleidoscope.variables])
    }

    fn scope_mut(kaleidoscope: &mut Kaleidoscope) -> ScopesMut<'_> {
        ScopesMut(vec![&mut kaleidoscope.variables])
    }
}

impl Scopeable for scopable::TypeVariable {
    fn scope(kaleidoscope: &Kaleidoscope) -> Scopes<'_> {
        Scopes(vec![&kaleidoscope.type_variables])
    }

    fn scope_mut(kaleidoscope: &mut Kaleidoscope) -> ScopesMut<'_> {
        ScopesMut(vec![&mut kaleidoscope.type_variables])
    }
}

impl Kaleidoscope {
    pub fn push<T: Scopeable>(&mut self) {
        T::scope_mut(self).push();
    }

    pub fn pop<T: Scopeable>(&mut self) {
        T::scope_mut(self).pop();
    }

    pub fn scope<T: Scopeable>(&mut self, fun: impl FnOnce(&mut Self)) {
        self.push::<T>();
        fun(self);
        self.pop::<T>();
    }

    pub fn add<T: Scopeable>(&mut self, name: Symbol) {
        for scope in T::scope_mut(self).0.iter_mut() {
            scope.map.last_mut().unwrap().insert(name.clone());
        }
    }

    pub fn contains<T: Scopeable>(&self, name: &Symbol) -> bool {
        T::scope(self)
            .0
            .iter()
            .any(|scope| scope.map.iter().any(|x| x.contains(name)))
    }
}
