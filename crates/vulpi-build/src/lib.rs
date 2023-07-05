//! Structures for compiling a [Crate] of [Source]s into a [Module]. The [Compiler] is the main
//! entry point for this library and it's used to keep that of the compilation process.

use vulpi_report::Reporter;
use vulpi_storage::id::{File, Id};
use vulpi_storage::vfs::FileSystem;
use vulpi_tree::Show;

pub mod error;
pub mod module;

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

        let lexer = vulpi_parser::Lexer::new(str);
        let mut parser = vulpi_parser::Parser::new(lexer, id);

        match parser.top_level() {
            Ok(x) => println!("{}", x.show()),
            Err(e) => println!("error {:?}", e),
        }

        Ok(id)
    }
}
