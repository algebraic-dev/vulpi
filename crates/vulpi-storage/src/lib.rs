use std::{collections::HashMap, marker::PhantomData};

use id::{Id, Identifier};
use std::hash::Hash;

pub mod file_system;
pub mod id;
pub mod interner;

pub struct Bag<U: Identifier, T> {
    pub map: Vec<T>,
    pub t_to_id: HashMap<T, Id<U>>,
    pub counter: usize,
    pub phantom: PhantomData<U>,
}

impl<U: Identifier + Clone, T: Hash + Eq + PartialEq + Clone> Bag<U, T> {
    pub fn insert(&mut self, value: T) -> Id<U> {
        if let Some(id) = self.t_to_id.get(&value) {
            return id.clone();
        }

        let id = Id::new(self.counter);
        self.counter += 1;
        self.map.push(value.clone());
        self.t_to_id.insert(value, id.clone());

        id
    }
}

impl<U: Identifier, T> Default for Bag<U, T> {
    fn default() -> Self {
        Self {
            map: Vec::new(),
            t_to_id: HashMap::new(),
            counter: 0,
            phantom: PhantomData,
        }
    }
}
