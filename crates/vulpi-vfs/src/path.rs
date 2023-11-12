use std::path::PathBuf;

use vulpi_intern::Symbol;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Vec<Symbol>,
}

impl Path {
    pub fn with(&self, new: Symbol) -> Path {
        let mut segments = self.segments.clone();
        segments.push(new);
        Path { segments }
    }

    pub fn to_pathbuf(&self, cwd: PathBuf) -> ::std::path::PathBuf {
        let mut path = cwd;
        
        for segment in &self.segments {
            path.push(segment.get());
        }

        path.set_extension(".vp");
        
        path
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Qualified {
    pub path: Path,
    pub name: Symbol,
}
