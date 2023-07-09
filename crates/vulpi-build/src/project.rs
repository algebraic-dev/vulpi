//! This module builds a project from a set of source files.

use std::{collections::HashMap, io::stdout, path::PathBuf};

use vulpi_report::renderer::{Classic, Renderer};
use vulpi_report::Report;
use vulpi_resolver::{Context, ModuleTree, Path};
use vulpi_storage::file_system::real::RealFileSystem;
use vulpi_storage::id::{self, Id};

use crate::error::HashReporter;

pub struct ProjectCompiler {
    reporter: Report,
    file_system: RealFileSystem,
    root: PathBuf,

    declared_modules: Vec<vulpi_resolver::Module>,
    interfaces: Vec<vulpi_resolver::Interface>,
    map: HashMap<Path, Id<id::Namespace>>,
    tree: ModuleTree,
}

impl ProjectCompiler {
    pub fn new(root: PathBuf) -> Self {
        Self {
            reporter: Report::new(HashReporter::new()),
            file_system: RealFileSystem::new(root.clone()),
            root,
            declared_modules: Default::default(),
            interfaces: Default::default(),
            map: HashMap::new(),
            tree: Default::default(),
        }
    }

    pub fn compile(&mut self, file: PathBuf) {
        let mut context = Context::new(
            self.reporter.clone(),
            "Main".into(),
            &mut self.file_system,
            &mut self.declared_modules,
            &mut self.interfaces,
            &mut self.map,
            &mut self.tree,
        );

        context.load_from_file("Main".into(), file).unwrap();

        if self.reporter.has_errors() {
            let mut stdout = stdout().lock();
            let renderer = Classic::new(&self.file_system, self.root.clone());
            for diagnostic in self.reporter.all_diagnostics() {
                diagnostic.render(&renderer, &mut stdout).unwrap();
            }
        }
    }
}
