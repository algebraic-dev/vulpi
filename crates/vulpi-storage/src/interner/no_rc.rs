use lazy_static::lazy_static;

use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::RwLock;

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

#[derive(Default)]
pub struct Interner {
    id_to_string: RwLock<Vec<String>>,
    string_to_id: RwLock<HashMap<String, Symbol>>,
    counter: AtomicUsize,
}

impl Interner {
    pub fn intern(&self, string: &str) -> Symbol {
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

    pub fn get(&self, id: &Symbol) -> Option<String> {
        self.id_to_string.read().unwrap().get(id.0).cloned()
    }
}

lazy_static! {
    static ref INTERNER: Interner = Interner::default();
}

pub fn intern(string: &str) -> Symbol {
    INTERNER.intern(string)
}
