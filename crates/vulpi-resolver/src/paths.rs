use vulpi_intern::Symbol;
use vulpi_syntax::r#abstract;

#[derive(Clone)]
pub struct Path {
    pub path: Vec<Symbol>,
}

impl Path {
    pub fn split_first(&self) -> Option<(Symbol, Path)> {
        if self.path.is_empty() {
            None
        } else {
            let (head, tail) = self.path.split_first().unwrap();
            Some((
                head.clone(),
                Path {
                    path: tail.to_vec(),
                },
            ))
        }
    }

    pub fn push(&mut self, name: Symbol) {
        self.path.push(name);
    }

    pub fn pop(&mut self) -> Option<Symbol> {
        self.path.pop()
    }

    pub fn slice(&self) -> &[Symbol] {
        &self.path
    }

    pub fn symbol(&self) -> Symbol {
        Symbol::intern(
            &self
                .path
                .iter()
                .map(|x| x.get())
                .collect::<Vec<String>>()
                .join("."),
        )
    }

    pub fn with(&self, name: Symbol) -> Path {
        let mut path = self.clone();
        path.push(name);
        path
    }

    pub fn qualify(&self, name: Symbol) -> Qualified {
        Qualified {
            path: self.clone(),
            name,
        }
    }
}

impl From<Qualified> for r#abstract::Qualified {
    fn from(value: Qualified) -> Self {
        r#abstract::Qualified {
            path: value.path.symbol(),
            name: value.name,
        }
    }
}

pub struct Qualified {
    pub path: Path,
    pub name: Symbol,
}
