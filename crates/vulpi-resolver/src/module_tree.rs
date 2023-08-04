//! This module declares a [Tree] that is responsible for declaring the modules and their
//! child.

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_show::{Show, TreeDisplay};

use crate::namespace::ModuleId;

/// A tree for modules. It starts with a single root module and then it can have multiple children
/// modules.
pub struct Tree {
    pub id: ModuleId,
    pub modules: HashMap<Symbol, Tree>,
}

impl Show for Tree {
    fn show(&self) -> vulpi_show::TreeDisplay {
        let mut display = vulpi_show::TreeDisplay::label(&format!("ModuleTree: {}", self.id.get()));
        let child = TreeDisplay::label("child");

        for (name, module) in &self.modules {
            display = display.with(TreeDisplay::label(&name.get()).with(module.show()));
        }

        display.with(child)
    }
}

impl Tree {
    pub fn new(id: ModuleId) -> Self {
        Self {
            id,
            modules: HashMap::new(),
        }
    }

    /// Adds a new entry to the tree (It only adds the entry if the last symbol is not present
    /// in the tree).
    pub fn add(&mut self, name: &[Symbol], id: ModuleId) -> Option<&mut Self> {
        if name.is_empty() {
            return None;
        }

        let (head, tail) = name.split_first().unwrap();

        if tail.is_empty() {
            Some(
                self.modules
                    .entry(head.clone())
                    .or_insert_with(|| Tree::new(id)),
            )
        } else {
            let module = self.modules.get_mut(head).unwrap();
            module.add(tail, id)
        }
    }

    /// Finds a mutable subtree in the tree.
    pub fn find_mut(&mut self, name: &[Symbol]) -> Option<&mut Tree> {
        let mut current = self;

        for symbol in name {
            current = current.modules.get_mut(symbol)?;
        }

        Some(current)
    }

    /// Finds a subtree in the tree.
    pub fn find(&self, name: &[Symbol]) -> Option<&Tree> {
        let mut current = self;

        for symbol in name {
            current = current.modules.get(symbol)?;
        }

        Some(current)
    }
}
