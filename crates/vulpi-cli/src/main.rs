use vulpi_lexer::Lexer;
use vulpi_parser::Parser;

use vulpi_report::renderer::classic::Classic;
use vulpi_report::renderer::Reader;
use vulpi_report::renderer::Renderer;
use vulpi_report::{hash::HashReporter, Report};

use vulpi_resolver::declare::Declare;
use vulpi_resolver::declare::ImportResolve;
use vulpi_resolver::module_tree::ModuleTree;
use vulpi_resolver::namespace::{ModuleId, Namespace};
use vulpi_resolver::Resolve;

use vulpi_vfs::real::RealFileSystem;
use vulpi_vfs::FileSystem;

use vulpi_typer::Declare as Decl;

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

    let mut tree = ModuleTree::new(ModuleId(0));
    let mut namespaces = vec![Namespace::new(None)];

    let mut resolver =
        vulpi_resolver::declare::Context::new(reporter.clone(), &mut tree, &mut namespaces);

    program.declare(&mut resolver);
    program.resolve_imports(&mut resolver);

    let size = namespaces.len();

    let mut ctx = vulpi_resolver::Context::new(reporter.clone(), namespaces, tree);

    let program = program.resolve(&mut ctx);

    let env = vulpi_typer::env::Env::new(reporter.clone(), size, ctx.prelude.clone());

    program.declare(env.clone());
    program.define(env);

    let report = reporter.all_diagnostics();

    if !reporter.has_errors() {
        println!("Ok!");
    } else {
        println!("Err!");
        let mut writer = Reader::default();
        let ctx = Classic::new(&vfs, cwd);

        for diagnostic in report {
            diagnostic.render(&ctx, &mut writer).unwrap();
        }

        print!("{}", writer.to_string());
    }
}
