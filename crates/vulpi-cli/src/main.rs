use std::collections::HashSet;
use std::path::PathBuf;

use vulpi_ir::compiler;
use vulpi_parser::parse;
use vulpi_report::hash_reporter;
use vulpi_report::renderer::classic::Classic;

use vulpi_report::{hash::HashReporter, Report};

use vulpi_resolver::error::ResolverErrorKind;
use vulpi_resolver::io::IO;
use vulpi_resolver::namespaces;
use vulpi_resolver::paths;
use vulpi_resolver::resolver;
use vulpi_resolver::scopes::Symbol;

use vulpi_syntax::concrete::tree::Program;

use vulpi_typer::type_checker;
use vulpi_vfs::real::RealFileSystem;
use vulpi_vfs::FileSystem;

pub struct Loader {
    pub cwd: PathBuf,
    pub reporter: Report,
    pub fs: RealFileSystem,
    pub loaded: HashSet<Symbol>,
}

impl Loader {
    pub fn new(cwd: PathBuf) -> Self {
        Self {
            cwd: cwd.clone(),
            reporter: Report::new(HashReporter::new()),
            fs: RealFileSystem::new(cwd),
            loaded: HashSet::new(),
        }
    }

    #[allow(clippy::result_unit_err)]
    pub fn start(&mut self) -> Result<Program, ()> {
        let Ok(file_id) = self.fs.load(self.cwd.join("main.vp")) else {
            return Err(())
        };

        let Ok(source) = self.fs.read(file_id) else {
            return Err(())
        };

        let program = parse(self.reporter.clone(), file_id, source);

        Ok(program)
    }
}

impl IO for Loader {
    fn read_module(&mut self, _id: paths::Path) -> Result<Program, ResolverErrorKind> {
        todo!()
    }
}

fn main() {
    let cwd = std::env::current_dir().unwrap();
    let mut loader = Loader::new(cwd.clone());
    let program = loader.start().unwrap();

    let reporter = loader.reporter.clone();
    let mut namespaces = namespaces();

    let program = resolver(reporter.clone(), &mut namespaces, &mut loader)
        .declare(&program)
        .import(&program)
        .resolve(program);

    let elaborated = type_checker(reporter.clone())
        .declare(&program)
        .define(&program)
        .output();

    if reporter.has_errors() {
        let ctx = Classic::new(&loader.fs, cwd);
        reporter.to_stderr(ctx)
    } else {
        let compiler = compiler().first_pass(elaborated);
    }
}
