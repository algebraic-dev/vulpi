use crate::resolve::Resolve;
use declare::Modules;
use resolve::Context;

use vulpi_report::Report;
use vulpi_storage::id::{self, Id};
use vulpi_storage::interner::Symbol;
use vulpi_syntax::{r#abstract::Program, resolved};

pub mod ambiguity;
pub mod declare;
pub mod error;
pub mod resolve;
pub mod scopes;

/// Main function of the module. It takes a program and resolves it.
pub fn resolve(
    program: Program,
    file_id: Id<id::File>,
    namespace: Id<id::Namespace>,
    reporter: Report,
    modules: &Modules,
) -> resolved::Program {
    let mut context = Context::new(reporter, file_id, namespace, modules);
    program.resolve(&mut context)
}

pub fn declare_main(context: &mut Modules, program: &mut Program) -> Id<id::Namespace> {
    declare::declare_main(context, program)
}

pub fn declare(
    context: &mut Modules,
    program: &mut Program,
    path: Vec<Symbol>,
) -> Id<id::Namespace> {
    declare::declare(context, program, path)
}
