//! Facilities to build a entire crate of vulpi files. This module is responsible for building the
//! crate from the source files and resolving the modules.


use std::{collections::HashMap, fs::File, path::PathBuf};


use resw::Writer;
use vulpi_intern::Symbol;
use vulpi_ir::transform;
use vulpi_location::{FileId, Span};
use vulpi_report::Report;
use vulpi_resolver::{dependencies::{Dependencies, self}, Module, Context, cycle::DepHolder};
use vulpi_syntax::concrete::tree::Program;
use vulpi_vfs::{FileSystem, path::Path};

pub mod real;

pub enum Interface {
    Compiled(Module, Dependencies),
    Uncompiled(Program),
}

pub struct ProjectCompiler<FS: FileSystem> {
    pub name: Symbol,
    pub fs: FS,
    pub reporter: Report,
}

impl<FS: FileSystem> ProjectCompiler<FS> {
    fn load(&mut self, _span: Span, path: FS::Path) -> Option<FileId> {
        if let Ok(id) = self.fs.load(path) {
            Some(id)
        } else {
            None
        }
    }

    fn parse(&mut self, id: FileId) -> Program {
        let source = self.fs.read(id).unwrap();
        vulpi_parser::parse(self.reporter.clone(), id, &source)
    }

    pub fn find_dependencies(&mut self, bag: &mut HashMap<Path, (Interface, Dependencies)>, deps: Dependencies) {
        for (path, span) in deps.imported {
            if !bag.contains_key(&path) {
                if let Some(id) = self.load(span.clone(), self.fs.from_src_path(path.clone())) {
                    let program = self.parse(id);
                    let deps = dependencies::dependencies(self.name.clone(), &program);
                    bag.insert(path.clone(), (Interface::Uncompiled(program), deps.clone()));
                    self.find_dependencies(bag, deps);
                } 
            } 
        }
    }
    
    pub fn compile(&mut self, module: Symbol, path: FS::Path, output: PathBuf) {
        // TODO: Fix this error :( I can't now because it would require changes
        // to the vulpi-report module. Good luck Sofia from the future!

        let root = self.fs.load(path).unwrap();
        let parsed = self.parse(root);

        let path = Path {
            segments: vec![module.clone(), Symbol::intern("Main")]
        };

        let mut bag = HashMap::new();
        let deps = dependencies::dependencies(self.name.clone(), &parsed);
        bag.insert(path.clone(), (Interface::Uncompiled(parsed), deps.clone()));

        self.find_dependencies(&mut bag, deps);

        let mut modules = HashMap::new();

        for (path, (program, deps)) in bag {
            match program {
                Interface::Compiled(module, _) => {
                    modules.insert(path, (module, None, deps));
                }
                Interface::Uncompiled(parsed) => {
                    let context = Context::new(path.clone(), self.reporter.clone());
                    let solved = vulpi_resolver::resolve(&context, parsed);
                    modules.insert(path, (context.module.clone(), Some((context, solved)), deps));
                },
            }
        }

        for (actual_module, _, _) in modules.values() {
            for (module, _, _) in modules.values() {
                let path = module.name().clone();
                actual_module.add_available(path, module.clone())
            }   
        } 

        let mut tc = vulpi_typer::Context::new(self.reporter.clone());
        let mut programs = vec![];

        let mut dep = DepHolder::default();
        
        for (_, ctx, _) in modules.into_values() {
            if let Some((ctx, resolver)) = ctx {
                let program = resolver.eval(ctx.clone());
                dep.register(&program);
                programs.push(program);
            }
        }

        dep.report_cycles(self.reporter.clone());

        let programs = vulpi_typer::Programs(programs);

        tc.declare(&programs);
        tc.define(&programs);

        let program = tc.elaborated;
        
        if !self.reporter.has_errors() {
            let res = transform::Transform::transform(program, &mut Default::default());

            let js = vulpi_js::Transform::transform(&res, &mut Default::default());
            let f = File::create(output).unwrap();
            let mut w = Writer::new(f);

            w.write_program(&js).unwrap();
        }
    }

}
