//! Structures for compiling a [Crate] of [Source]s into a [Module]. The [Compiler] is the main
//! entry point for this library and it's used to keep that of the compilation process.

use std::io::stderr;
use std::path::PathBuf;

use vulpi_report::renderer::{Classic, Renderer};
use vulpi_report::{Report, Reporter};
use vulpi_resolver::context;
use vulpi_storage::vfs::FileSystem;

pub mod error;
pub mod module;

pub struct Instance {
    pub resolver: context::Context,
    pub reporter: Report,
}

pub enum Exit {
    Ok,
    Err,
}

impl Instance {
    pub fn new(
        file_system: impl FileSystem<PathBuf, String> + 'static,
        reporter: impl Reporter + 'static,
    ) -> Self {
        let reporter = Report::new(reporter);
        Self {
            resolver: context::Context::new(file_system, reporter.clone()),
            reporter,
        }
    }

    pub fn render_errors(&mut self) {
        let classic_renderer = Classic::new(
            &*self.resolver.file_system,
            std::env::current_dir().unwrap(),
        );

        for diagnostic in self.reporter.all_diagnostics() {
            diagnostic
                .render(&classic_renderer, &mut stderr().lock())
                .unwrap();
        }
    }

    pub fn compile(&mut self, source: PathBuf) -> Result<(), vulpi_storage::vfs::Error> {
        let path = ["Main"].as_slice().into();
        self.resolver.create_from_file(path, source)?;

        if self.reporter.has_errors() {
            self.render_errors();
        }

        Ok(())
    }
}
