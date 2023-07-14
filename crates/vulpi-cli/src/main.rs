use clap::Parser;
use std::{
    io::{stderr, stdout},
    path::PathBuf,
};
use vulpi_build::error::HashReporter;
use vulpi_parser::{parse, Lexer};
use vulpi_report::{
    renderer::{Classic, Renderer},
    Report,
};
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
    let _ = parse(lexer, file, reporter.clone());

    if reporter.has_errors() {
        eprintln!();

        let renderer = Classic::new(&fs, cwd_real);

        let diagnostics = reporter.all_diagnostics();
        for diagnostic in diagnostics {
            diagnostic.render(&renderer, &mut stderr().lock()).unwrap();
        }
    }
}
