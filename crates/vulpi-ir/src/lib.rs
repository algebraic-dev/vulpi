//! Intermediate representation of a Vulpi program.

use std::collections::HashMap;

use crate::first_pass::FirstPass;

use vulpi_intern::Symbol;
use vulpi_show::Show;
use vulpi_syntax::{elaborated::*, r#abstract::Qualified};
use vulpi_typer::{Real, Type};

pub mod first_pass;
pub mod ir;
pub mod pattern;

#[derive(Default)]
pub struct Context {
    pub enums: HashMap<Qualified, Vec<(Qualified, usize)>>,
    pub records: HashMap<Qualified, Vec<Qualified>>,
    pub fields: HashMap<Qualified, (Qualified, usize)>,
    pub constructors: HashMap<Qualified, (Qualified, usize, usize)>,
    pub eff_types: HashMap<Qualified, Vec<Qualified>>,
    pub effects: HashMap<Qualified, (Qualified, usize)>,
    pub bound: HashMap<Symbol, usize>,
    pub counter: usize,
}

impl Context {
    pub fn add_bound(&mut self, name: Symbol) {
        let count = self.bound.entry(name).or_insert(0);
        *count += 1;
    }

    pub fn fresh(&mut self) -> Symbol {
        self.counter += 1;
        Symbol::Generated(self.counter)
    }

    pub fn remove_bound(&mut self, name: Symbol) {
        let count = self.bound.entry(name.clone()).or_insert(0);
        *count -= 1;

        if *count == 0 {
            self.bound.remove(&name);
        }
    }

    pub fn is_bound(&self, name: Symbol) -> bool {
        self.bound.contains_key(&name)
    }

    pub fn first_pass(mut self, program: Program<Type<Real>>) -> Self {
        let result = program.first_pass(&mut self);
        println!("{}", result.show());
        self
    }
}

pub fn compiler() -> Context {
    Context::default()
}
