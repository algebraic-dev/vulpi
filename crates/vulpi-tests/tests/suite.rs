#![feature(custom_test_frameworks)]
#![test_runner(vulpi_tests::test_runner)]

use std::cell::RefCell;
use std::rc::Rc;

use vulpi_build::error::HashReporter;
use vulpi_parser::{parse, Lexer};
use vulpi_report::renderer::Classic;
use vulpi_report::renderer::Reader;
use vulpi_report::renderer::Renderer;
use vulpi_report::Report;
use vulpi_resolver::declare::{self, Modules};
use vulpi_storage::file_system::real::RealFileSystem;
use vulpi_storage::file_system::FileSystem;
use vulpi_tests::test;
use vulpi_typer::context::Env;

test!("/suite/", |file_name| {
    let cwd_real = std::env::current_dir().unwrap();

    let mut fs = RealFileSystem::new(file_name.clone());
    let reporter = Report::new(HashReporter::new());

    let file = fs.load(file_name).unwrap();
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

    vulpi_typer::declare::define_body(&env, &resolved);

    let mut end = Reader::default();

    if reporter.has_errors() {
        eprintln!();

        let renderer = Classic::new(&fs, cwd_real);

        let diagnostics = reporter.all_diagnostics();
        for diagnostic in diagnostics {
            diagnostic.render(&renderer, &mut end).unwrap();
        }
    }

    end.to_string()
});
