//! Facilities to build a entire crate of vulpi files. This module is responsible for building the
//! crate from the source files and resolving the modules.

use vulpi_vfs::FileSystem;

pub struct ProjectCompiler<FS: FileSystem> {
    fs: FS,
}

impl<FS: FileSystem> ProjectCompiler<FS> {
    fn parse(&mut self) {
        
    }
    
    pub fn compile(&mut self) {
        
    }

    pub fn check(&mut self) {
        
    }
}