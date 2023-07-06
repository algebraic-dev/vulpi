use clap::Parser;
use std::path::PathBuf;
use vulpi_build::{error::HashReporter, Exit, Instance};
use vulpi_storage::vfs::real::RealFileSystem;

#[derive(Parser)]
enum Cli {
    TypeCheck { file: PathBuf },
}

fn main() {
    let Cli::TypeCheck { file } = Cli::parse();

    let cwd = std::env::current_dir().unwrap();
    let file_system = RealFileSystem::new(cwd);

    let reporter = HashReporter::new();

    let mut instance = Instance::new(file_system, reporter);

    let exit = instance.compile(file).unwrap();

    match exit {
        Exit::Ok => std::process::exit(0),
        Exit::Err => std::process::exit(1),
    }
}
