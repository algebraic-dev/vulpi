#![feature(custom_test_frameworks)]
#![test_runner(vulpi_tests::test_runner)]

use vulpi_lexer::Lexer;
use vulpi_parser::Parser;
use vulpi_report::{
    hash::HashReporter,
    renderer::{classic::Classic, Reader, Renderer},
    Report,
};
use vulpi_resolver::declare::Declare;
use vulpi_resolver::declare::ImportResolve;
use vulpi_resolver::{module_tree::ModuleTree, namespace::ModuleId};
use vulpi_show::Show;
use vulpi_tests::test;
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

    let mut tree = ModuleTree::new(ModuleId(0));
    let mut resolver = vulpi_resolver::declare::Context::new(reporter.clone(), &mut tree);

    program.declare(&mut resolver);
    program.resolve_imports(&mut resolver);

    let report = reporter.all_diagnostics();

    if !reporter.has_errors() {
        tree.show().to_string()
    } else {
        let mut writer = Reader::default();
        let ctx = Classic::new(&vfs, cwd);

        for diagnostic in report {
            diagnostic.render(&ctx, &mut writer).unwrap();
        }

        writer.to_string()
    }
});
