//! Structures for compiling a [Crate] of [Source]s into a [Module]. The [Compiler] is the main
//! entry point for this library and it's used to keep that of the compilation process.

use std::path::PathBuf;

use vulpi_report::Reporter;
use vulpi_storage::{
    id::{File, Id},
    vfs::FileSystem,
};

pub mod error;
pub mod module;

/// A source file that can be compiled into a [Module]. It's the main input of the [Compiler]
pub struct Source(Id<File>);

pub struct Config {}

pub struct Instance<P> {
    pub file_system: Box<dyn FileSystem<P, Vec<u8>>>,
    pub reporter: Box<dyn Reporter>,
}

impl<P> Instance<P> {
    pub fn new(
        file_system: impl FileSystem<P, Vec<u8>> + 'static,
        reporter: impl Reporter + 'static,
    ) -> Self {
        Self {
            file_system: Box::new(file_system),
            reporter: Box::new(reporter),
        }
    }

    pub fn compile(&mut self, source: P) -> Result<Id<File>, vulpi_storage::vfs::Error> {
        let id = self.file_system.load(source)?;

        let content = self.file_system.read(id)?;

        let str = std::str::from_utf8(content).unwrap();

        let mut parser = vulpi_parser::Parser::new(str, id, &mut *self.reporter);
        parser.root();

        let parsed = parser.finish();

        Ok(id)
    }
}
