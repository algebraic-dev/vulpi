pub mod real;

use std::error::Error;

pub trait FileSystem {
    type Path;
    fn write(&self, path: Self::Path, content: Vec<u8>);
    fn read(&self, path: Self::Path) -> Result<Vec<u8>, Box<dyn Error>>; 
}