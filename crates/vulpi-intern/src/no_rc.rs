//! A simple string interner with no reference counting so it lives until the end of the program.

use lazy_static::lazy_static;
use vulpi_show::Show;

use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::RwLock;

lazy_static! {
    static ref INTERNER: Interner = Interner::default();
}

/// A symbol is a reference to a string inside the interner. It is used to compare strings by
/// comparing their ids instead of comparing their content because it is more efficient (it makes
/// the comparison an integer comparison instead of a string comparison).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(usize);

impl Symbol {
    pub fn intern(string: &str) -> Self {
        INTERNER.intern(string)
    }

    pub fn get(&self) -> String {
        INTERNER.get(self).unwrap()
    }
}

impl Show for Symbol {
    fn show(&self) -> vulpi_show::TreeDisplay {
        vulpi_show::TreeDisplay::label(&format!("Symbol: {}", self.get()))
    }
}
#[derive(Default)]
struct Interner {
    id_to_string: RwLock<Vec<String>>,
    string_to_id: RwLock<HashMap<String, Symbol>>,
    counter: AtomicUsize,
}

impl Interner {
    fn intern(&self, string: &str) -> Symbol {
        if let Some(id) = self.string_to_id.read().unwrap().get(string) {
            return id.clone();
        }

        let mut id_to_string = self.id_to_string.write().unwrap();
        let mut string_to_id = self.string_to_id.write().unwrap();

        let id = Symbol(self.counter.fetch_add(1, Ordering::SeqCst));
        id_to_string.push(string.to_owned());
        string_to_id.insert(string.to_owned(), id.clone());

        id
    }

    fn get(&self, id: &Symbol) -> Option<String> {
        self.id_to_string.read().unwrap().get(id.0).cloned()
    }
}
