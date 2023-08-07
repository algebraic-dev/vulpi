use std::collections::{HashMap, HashSet};

use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract::Qualified;

use crate::{env::Env, types::Type};

#[derive(Default)]
pub struct Ambient {
    pub effects: HashMap<Qualified, Type>,
}

impl Ambient {
    pub fn causes(&mut self, effect: Qualified, cause: Type) {
        self.effects.insert(effect, cause);
    }

    pub fn to_type(&self) -> Type {
        let mut effects = Vec::new();

        for (effect, cause) in &self.effects {
            effects.push((effect.clone(), cause.clone()));
        }

        effects
            .into_iter()
            .fold(Type::lacks(HashSet::new()), |rest, (label, ty)| {
                Type::extend_row(label, ty, rest)
            })
    }
}
