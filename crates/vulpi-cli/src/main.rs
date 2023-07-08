use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
enum Cli {
    TypeCheck { file: PathBuf },
}

fn main() {
    let Cli::TypeCheck { file } = Cli::parse();

    todo!()
}
