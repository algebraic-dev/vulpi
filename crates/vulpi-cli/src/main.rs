use std::{env, path::PathBuf};

use vulpi_build::real::RealFileSystem;
use vulpi_intern::Symbol;
use vulpi_report::renderer::classic::Classic;


fn main() {
    let cwd = env::current_dir().unwrap();

    let name = Symbol::intern("Yal");

    let mut compiler = vulpi_build::ProjectCompiler {
        fs: RealFileSystem::new(name.clone(), cwd.clone(), cwd.clone().join("build")),
        reporter: vulpi_report::hash_reporter(),
        name: name.clone()
    };

    compiler.compile(name.clone(), PathBuf::from("Main.vp"));
    
    let ctx = Classic::new(&compiler.fs, cwd.clone());
    compiler.reporter.to_stderr(ctx)
}
