use clap::Parser;
use std::{io::stderr, path::PathBuf};

use vulpi_build::error::HashReporter;
use vulpi_parser::{parse, Lexer};
use vulpi_report::renderer::{Classic, Renderer};
use vulpi_report::Report;
use vulpi_show::Show;
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

    let mut desugared = vulpi_desugar::desugar(concrete, file, reporter.clone());
    let mut modules = vulpi_resolver::declare::Modules::default();
    let namespace = vulpi_resolver::declare::declare_main(&mut modules, &mut desugared);
    let resolved = vulpi_resolver::resolve(desugared, file, namespace, reporter.clone(), &modules);

    println!("{}", resolved.show());

    if reporter.has_errors() {
        eprintln!();

        let renderer = Classic::new(&fs, cwd_real);

        let diagnostics = reporter.all_diagnostics();
        for diagnostic in diagnostics {
            diagnostic.render(&renderer, &mut stderr().lock()).unwrap();
        }
    }
}
