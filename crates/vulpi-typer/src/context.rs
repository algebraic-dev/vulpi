//! This file declares a mutable environment that is useful to keep track of information that does
//! not need to be immutable like the Env.

use vulpi_intern::Symbol;

use crate::r#type::{r#virtual::Env, r#virtual::Virtual, Type};

/// A mutable context that is used differently from [Env]. It is used to keep data between every
/// thing inside the type checker.
pub struct Context {
    pub counter: usize,
}

impl Context {
    fn inc_counter(&mut self) -> usize {
        self.counter += 1;
        self.counter - 1
    }

    /// Creates a new name with the prefix `t_` and a unique number.
    pub fn new_name(&mut self) -> Symbol {
        Symbol::intern(&format!("t_{}", self.inc_counter()))
    }

    /// Creates a new hole that is a type that is not yet known
    pub fn hole(&mut self, env: &Env) -> Type<Virtual> {
        env.hole(self.new_name())
    }

    /// Creates a "lacks" hole that stores effects that should lack.
    pub fn lacks(&mut self, env: &Env) -> Type<Virtual> {
        env.lacks(self.new_name())
    }
}
