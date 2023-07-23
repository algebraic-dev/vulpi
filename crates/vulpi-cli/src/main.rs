use clap::Parser;
use std::cell::RefCell;
use std::rc::Rc;
use std::{io::stderr, path::PathBuf};
use vulpi_resolver::declare::{self, Modules};
use vulpi_typer::context::Env;

use vulpi_build::error::HashReporter;
use vulpi_parser::{parse, Lexer};
use vulpi_report::renderer::{Classic, Renderer};
use vulpi_report::Report;
use vulpi_storage::file_system::{real::RealFileSystem, FileSystem};

#[derive(Parser)]
enum Cli {
    TypeCheck { file: PathBuf },
}

fn main() {
    let Cli::TypeCheck { file } = Cli::parse();

    let cwd_real = std::env::current_dir().unwrap();

    let mut fs = RealFileSystem::new(cwd_real.clone());
    let reporter = Report::new(HashReporter::new());

    let file = fs.load(file).unwrap();
    let source = fs.read(file).unwrap();

    let lexer = Lexer::new(source);
    let concrete = parse(lexer, file, reporter.clone());

    let mut desugared = vulpi_desugar::desugar(concrete);
    let mut modules = Modules::new(reporter.clone(), file);
    let namespace = declare::declare_main(&mut modules, &mut desugared);
    let resolved = vulpi_resolver::resolve(desugared, file, namespace, reporter.clone(), &modules);

    let mut modules = vulpi_typer::Modules::default();

    vulpi_typer::declare::declare_types(&mut modules, &resolved);

    let modules = Rc::new(RefCell::new(modules));
    let env = Env::new(reporter.clone(), file, modules);

    vulpi_typer::declare::declare_values_types(env.clone(), &resolved);

    vulpi_typer::declare::define_let_body(&env, &resolved);

    if reporter.has_errors() {
        eprintln!();

        let renderer = Classic::new(&fs, cwd_real);

        let diagnostics = reporter.all_diagnostics();
        for diagnostic in diagnostics {
            diagnostic.render(&renderer, &mut stderr().lock()).unwrap();
        }
    }
}
