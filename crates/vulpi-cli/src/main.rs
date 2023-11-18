#![feature(panic_info_message)]
#![feature(panic_can_unwind)]

use std::{env, path::PathBuf, panic, backtrace::Backtrace};

use vulpi_build::real::RealFileSystem;
use vulpi_intern::Symbol;
use vulpi_report::renderer::classic::Classic;

use clap::Parser;

#[derive(Parser)]
enum Cli {
    Compile {
        package: String,
        file_name: String,

        #[clap(short, long)]
        output: Option<String>
    }
}

fn main() {
    panic::set_hook(Box::new(|e| {
        eprintln!(
            "\n[Error]: internal compiler error '{:?}' at {}",
            e.message().unwrap(),
            e.location().unwrap()
        );
        eprintln!("-  It should not occur. Please submit an issue to the Vulpi repository:)");
        eprintln!("-  Here: https://github.com/lang-vulpi/vulpi/issues\n");

        if std::env::var("RUST_BACKTRACE").is_ok() {
            let backtrace = Backtrace::capture();

            eprintln!("Stack trace: \n{}", backtrace)
        }
    }));

    let result = Cli::parse();
    
    match result {
        Cli::Compile { file_name, package , output } => {
            let cwd = env::current_dir().unwrap();

            let name = Symbol::intern(&package);

            let output = output.unwrap_or_else(|| format!("{}.js", file_name.split(".").next().unwrap().to_string()));
        
            let mut compiler = vulpi_build::ProjectCompiler {
                fs: RealFileSystem::new(name.clone(), cwd.clone(), cwd.clone().join("build")),
                reporter: vulpi_report::hash_reporter(),
                name: name.clone()
            };
        
            compiler.compile(name.clone(), PathBuf::from(file_name), PathBuf::from(output));
            
            let ctx = Classic::new(&compiler.fs, cwd.clone());
            compiler.reporter.to_stderr(ctx)
        }
    }
}
