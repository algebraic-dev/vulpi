use std::{path, fs};

use crate::FileSystem;

pub struct RealFileSystem;
impl FileSystem for RealFileSystem {
    type Path = path::PathBuf;
    
    fn write(&self, path: Self::Path, content: Vec<u8>) {
        fs::write(path, content).unwrap();
    }

    fn read(&self, path: Self::Path) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        fs::read(path).map_err(|_| "Cannot read".into()) 
    }
}