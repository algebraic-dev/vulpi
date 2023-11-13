use std::{path::PathBuf, fmt::{Formatter, Display, Error}};

use vulpi_intern::Symbol;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Vec<Symbol>,
}

impl Path {
    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }
    
    pub fn with(&self, new: Symbol) -> Path {
        let mut segments = self.segments.clone();
        segments.push(new);
        Path { segments }
    }

    pub fn symbol(&self) -> Symbol {
        Symbol::intern(&self.segments.iter().map(|x| x.get()).collect::<Vec<_>>().join("."))
    }

    pub fn shift(&self) -> Path {
        let mut segments = self.segments.clone();
        segments.remove(0);
        Path { segments }
    }

    pub fn to_pathbuf(&self, cwd: PathBuf) -> ::std::path::PathBuf {
        let mut path = cwd;
        
        for segment in &self.segments {
            path.push(segment.get());
        }

        path.set_extension("vp");
        
        path
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        for (i, segment) in self.segments.iter().enumerate() {
            if i != 0 {
                write!(f, ".")?;
            }

            write!(f, "{}", segment.get())?;
        }

        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Qualified {
    pub path: Path,
    pub name: Symbol,
}
