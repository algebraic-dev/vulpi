use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_syntax::concrete::{self, tree::TopLevel, Upper};
use vulpi_vfs::path::Path;

#[derive(Clone)]
pub struct Dependencies {
    pub declared: Vec<Path>,
    pub imported: Vec<(Path, Span)>,
    pub opened: Vec<Path>,
}

pub fn from_path_upper(path: &concrete::Path<Upper>) -> Path {
    let mut path_result = Path { segments: vec![] };

    for segment in &path.segments {
        path_result.segments.push(segment.0.symbol());
    }

    path_result.segments.push(path.last.symbol());

    path_result
}

pub fn dependencies(root: Symbol, program: &concrete::tree::Program) -> Dependencies {
    pub fn dependencies(
        root: Symbol,
        path: Vec<Symbol>,
        program: &[TopLevel],
        deps: &mut Dependencies,
    ) {
        for top_level in program {
            match top_level {
                concrete::tree::TopLevel::Use(use_) => {
                    let path = from_path_upper(&use_.path);
                    deps.imported.push((path.clone(), use_.path.span.clone()));

                    if use_.alias.is_none() {
                        deps.opened.push(path);
                    }
                }
                concrete::tree::TopLevel::Module(decl) => {
                    let mut path = path.clone();
                    path.push(decl.name.symbol());
                    if let Some(res) = &decl.part {
                        dependencies(root.clone(), path, &res.top_levels, deps);
                    } else {
                        deps.declared.push(Path { segments: path });
                    }
                }
                _ => (),
            }
        }
    }

    let mut deps = Dependencies {
        declared: Vec::new(),
        imported: Vec::new(),
        opened: Vec::new(),
    };

    dependencies(
        root.clone(),
        vec![root.clone()],
        &program.top_levels,
        &mut deps,
    );

    deps
}
