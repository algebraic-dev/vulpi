//! A simple string interner with no reference counting so it lives until the end of the program.

use vulpi_show::Show;

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

thread_local! {
    static INTERNER: Interner = Interner::default();
}

/// A symbol is a reference to a string inside the interner. It is used to compare strings by
/// comparing their ids instead of comparing their content because it is more efficient (it makes
/// the comparison an integer comparison instead of a string comparison).
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
    Generated(usize),
    Interned(usize),
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get())
    }
}

impl Symbol {
    pub fn intern(string: &str) -> Self {
        INTERNER.with(|i| i.intern(string))
    }

    pub fn get(&self) -> String {
        INTERNER.with(|i| i.get(self).unwrap())
    }
}

impl Show for Symbol {
    fn show(&self) -> vulpi_show::TreeDisplay {
        vulpi_show::TreeDisplay::label(&format!("Symbol: {}", self.get()))
    }
}
#[derive(Default)]
struct Interner {
    id_to_string: RefCell<Vec<String>>,
    string_to_id: RefCell<HashMap<String, Symbol>>,
    counter: AtomicUsize,
}

impl Interner {
    fn intern(&self, string: &str) -> Symbol {
        if let Some(id) = self.string_to_id.borrow().get(string) {
            return id.clone();
        }

        let mut id_to_string = self.id_to_string.borrow_mut();
        let mut string_to_id = self.string_to_id.borrow_mut();

        let id = Symbol::Interned(self.counter.fetch_add(1, Ordering::SeqCst));
        id_to_string.push(string.to_owned());
        string_to_id.insert(string.to_owned(), id.clone());

        id
    }

    fn get(&self, id: &Symbol) -> Option<String> {
        match id {
            Symbol::Generated(n) => Some(format!("%{n}")),
            Symbol::Interned(id) => self.id_to_string.borrow().get(*id).cloned(),
        }
    }
}
