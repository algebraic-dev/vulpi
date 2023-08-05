pub mod scopable {
    pub enum Variable {}
    pub enum TypeVariable {}
}

pub use vulpi_intern::Symbol;

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

impl Scope {
    pub fn push(&mut self) {
        self.map.push(Default::default());
    }

    pub fn pop(&mut self) {
        self.map.pop();
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

pub trait Scopable {
    fn scope(kaleidoscope: &mut Kaleidoscope) -> &mut Scope;
}

impl Scopable for scopable::Variable {
    fn scope(kaleidoscope: &mut Kaleidoscope) -> &mut Scope {
        &mut kaleidoscope.variables
    }
}

impl Scopable for scopable::TypeVariable {
    fn scope(kaleidoscope: &mut Kaleidoscope) -> &mut Scope {
        &mut kaleidoscope.type_variables
    }
}

impl Kaleidoscope {
    pub fn push<T: Scopable>(&mut self) {
        T::scope(self).push();
    }

    pub fn pop<T: Scopable>(&mut self) {
        T::scope(self).pop();
    }

    pub fn scope<T: Scopable>(&mut self, fun: impl FnOnce(&mut Self)) {
        self.push::<T>();
        fun(self);
        self.pop::<T>();
    }

    pub fn add<T: Scopable>(&mut self, name: Symbol) {
        T::scope(self).map.last_mut().unwrap().insert(name);
    }

    pub fn contains<T: Scopable>(&mut self, name: &Symbol) -> bool {
        T::scope(self)
            .map
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }
}
