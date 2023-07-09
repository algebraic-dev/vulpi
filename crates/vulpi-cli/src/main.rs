use clap::Parser;
use std::path::PathBuf;
use vulpi_build::project::ProjectCompiler;

#[derive(Parser)]
enum Cli {
    TypeCheck { file: PathBuf },
}

fn main() {
    let Cli::TypeCheck { file } = Cli::parse();

    let root = std::env::current_dir().unwrap();

    ProjectCompiler::new(root).compile(file);
}
