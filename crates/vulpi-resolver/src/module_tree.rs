//! This module declares a [ModuleTree] that is responsible for declaring the modules and their
//! child.

use std::collections::HashMap;

use vulpi_intern::Symbol;
use vulpi_show::{Show, TreeDisplay};

use crate::namespace::{ModuleId, Namespace};

/// A tree for modules. It starts with a single root module and then it can have multiple children
/// modules.
pub struct ModuleTree {
    pub id: ModuleId,
    pub namespace: Namespace,
    pub modules: HashMap<Symbol, ModuleTree>,
}

impl Show for ModuleTree {
    fn show(&self) -> vulpi_show::TreeDisplay {
        let mut display = vulpi_show::TreeDisplay::label(&format!("ModuleTree: {}", self.id.0));

        let namespace = self.namespace.show();
        let child = TreeDisplay::label("child");

        for (name, module) in &self.modules {
            display = display.with(TreeDisplay::label(&name.get()).with(module.show()));
        }

        display.with(namespace).with(child)
    }
}

impl ModuleTree {
    pub fn new(id: ModuleId) -> Self {
        Self {
            id,
            namespace: Namespace::default(),
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
                    .or_insert_with(|| ModuleTree::new(id)),
            )
        } else {
            let module = self.modules.get_mut(head).unwrap();
            module.add(tail, id)
        }
    }

    /// Finds a subtree in the tree.
    pub fn find(&mut self, name: &[Symbol]) -> Option<&mut ModuleTree> {
        let mut current = self;

        for symbol in name {
            current = current.modules.get_mut(symbol)?;
        }

        Some(current)
    }
}
