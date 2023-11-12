//! Facilities to build a entire crate of vulpi files. This module is responsible for building the
//! crate from the source files and resolving the modules.

use error::BuildError;
use vulpi_lexer::Lexer;
use vulpi_location::Span;
use vulpi_report::{Report, Diagnostic};
use vulpi_vfs::{FileSystem, path::Path};

mod error;

pub struct ProjectCompiler<FS: FileSystem> {
    fs: FS,
    reporter: Report,
}

impl<FS: FileSystem> ProjectCompiler<FS> {
    fn load(&mut self, span: Span, path: FS::Path) {
        match self.fs.load(path) {
            Ok(ok) => {
                let content = self.fs.read(ok).unwrap();
                let lexer = Lexer::new(content);
                let tokens = lexer.collect::<Vec<_>>();
            },
            Err(_) => {
                self.reporter.report(Diagnostic::new(BuildError {
                    span,
                    kind: error::BuildErrorKind::NotFound
                }))
            },
        }
    }
    
    fn parse(&mut self, source: Span, path: FS::Path) {
        
    }

    pub fn compile(&mut self) {
        
    }

    pub fn check(&mut self) {
        
    }
}