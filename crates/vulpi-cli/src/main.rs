use std::collections::HashMap;

use vulpi_lexer::Lexer;
use vulpi_parser::Parser;

use vulpi_report::renderer::classic::Classic;
use vulpi_report::renderer::Reader;
use vulpi_report::renderer::Renderer;
use vulpi_report::{hash::HashReporter, Report};

use vulpi_resolver::declare::Declare;
use vulpi_resolver::declare::ImportResolve;
use vulpi_resolver::module_tree::Tree;
use vulpi_resolver::namespace::Namespace;
use vulpi_resolver::namespace::Namespaces;
use vulpi_resolver::scopes::Symbol;
use vulpi_resolver::Resolve;

use vulpi_typer::Declare as Decl;

use vulpi_vfs::real::RealFileSystem;
use vulpi_vfs::FileSystem;

fn main() {
    let file_name = std::env::args().nth(1).unwrap();

    let reporter = Report::new(HashReporter::new());
    let cwd = std::env::current_dir().unwrap();

    let mut vfs = RealFileSystem::new(cwd.clone());
    let id = vfs.load(file_name.into()).unwrap();
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
    let program = program.resolve(&mut resolver);

    let env = vulpi_typer::Env::default();
    let mut ctx = vulpi_typer::Context::new(reporter.clone());

    program.declare((&mut ctx, env));

    // program.define(env);

    let report = reporter.all_diagnostics();

    if !reporter.has_errors() {
        println!("Ok!")
    } else {
        eprintln!();

        let mut writer = Reader::default();
        let ctx = Classic::new(&vfs, cwd);

        for diagnostic in report.iter().rev() {
            diagnostic.render(&ctx, &mut writer).unwrap();
        }

        eprint!("{}", writer.to_string());
    }
}
