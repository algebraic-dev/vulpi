#![feature(custom_test_frameworks)]
#![test_runner(vulpi_tests::test_runner)]

use std::collections::HashMap;

use vulpi_lexer::Lexer;
use vulpi_parser::Parser;
use vulpi_report::{
    hash::HashReporter,
    renderer::{classic::Classic, Reader, Renderer},
    Report,
};
use vulpi_resolver::namespace::Namespaces;
use vulpi_resolver::Resolve;
use vulpi_resolver::{declare::Declare, namespace::Namespace};
use vulpi_resolver::{declare::ImportResolve, scopes::Symbol};
use vulpi_resolver::{module_tree::Tree, namespace::ModuleId};
use vulpi_show::Show;
use vulpi_tests::test;
use vulpi_typer::Declare as Decl;

use vulpi_vfs::{real::RealFileSystem, FileSystem};

test!("/suite", |file_name| {
    let reporter = Report::new(HashReporter::new());
    let cwd = std::env::current_dir().unwrap();

    let mut vfs = RealFileSystem::new(cwd.clone());
    let id = vfs.load(file_name).unwrap();
    let source = vfs.read(id).unwrap();

    let lexer = Lexer::new(source, id, reporter.clone());

    let mut parser = Parser::new(lexer, id, reporter.clone());
    let program = parser.program();

    let tree = Tree::new(Symbol::intern(""));
    let mut namespaces = HashMap::new();

    namespaces.insert(Symbol::intern(""), Namespace::new(Symbol::intern("")));

    let mut namespaces = Namespaces { tree, namespaces };

    let mut resolver = vulpi_resolver::Context::new(reporter.clone(), &mut namespaces);

    program.declare(&mut resolver);
    program.resolve_imports(&mut resolver);

    // let mut ctx = vulpi_resolver::Context::new(reporter.clone(), namespaces, tree);
    //
    let program = program.resolve(&mut resolver);
    //
    // let env = vulpi_typer::env::Env::new(reporter.clone());
    //
    // program.declare(env.clone());
    // program.define(env);
    //
    let report = reporter.all_diagnostics();

    if !reporter.has_errors() {
        program.show().to_string()
    } else {
        let mut writer = Reader::default();
        let ctx = Classic::new(&vfs, cwd);

        for diagnostic in report.iter().rev() {
            diagnostic.render(&ctx, &mut writer).unwrap();
        }

        writer.to_string()
    }
});
