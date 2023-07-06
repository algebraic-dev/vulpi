//! Structures for compiling a [Crate] of [Source]s into a [Module]. The [Compiler] is the main
//! entry point for this library and it's used to keep that of the compilation process.

use std::io::stderr;
use std::path::PathBuf;

use vulpi_report::renderer::{Classic, Renderer};
use vulpi_report::{Diagnostic, Reporter};
use vulpi_storage::vfs::FileSystem;
use vulpi_tree::Show;

pub mod error;
pub mod module;

pub struct Config {}

pub struct Instance<P> {
    pub file_system: Box<dyn FileSystem<P, String>>,
    pub reporter: Box<dyn Reporter>,
}

pub enum Exit {
    Ok,
    Err,
}

impl Instance<PathBuf> {
    pub fn new(
        file_system: impl FileSystem<PathBuf, String> + 'static,
        reporter: impl Reporter + 'static,
    ) -> Self {
        Self {
            file_system: Box::new(file_system),
            reporter: Box::new(reporter),
        }
    }

    pub fn compile(&mut self, source: PathBuf) -> Result<Exit, vulpi_storage::vfs::Error> {
        let id = self.file_system.load(source)?;
        let str = self.file_system.read(id)?;

        let lexer = vulpi_parser::Lexer::new(str);
        let mut parser = vulpi_parser::Parser::new(lexer, id);

        match parser.program() {
            Ok(x) => {
                println!("{}", x.show());
                Ok(Exit::Ok)
            }
            Err(e) => {
                let classic_renderer =
                    Classic::new(&*self.file_system, std::env::current_dir().unwrap());

                let diagnostic: Diagnostic = Box::new(e);

                diagnostic
                    .render(&classic_renderer, &mut stderr().lock())
                    .unwrap();
                Ok(Exit::Err)
            }
        }
    }
}
