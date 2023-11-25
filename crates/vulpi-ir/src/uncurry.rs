use vulpi_intern::Symbol;
use vulpi_syntax::{
    lambda::LetDecl,
    lambda::{self, Program},
    r#abstract::Qualified,
};

pub fn accumulate_lambda_nodes<'a>(
    expr: &'a mut lambda::ExprKind,
    params: &mut Vec<Symbol>,
) -> &'a mut lambda::ExprKind {
    match expr {
        lambda::ExprKind::Lambda(param, body) => {
            params.extend(param.clone());
            accumulate_lambda_nodes(body, params)
        }
        e => e,
    }
}

pub fn create_big_lambda<'a>(
    expr: &'a mut lambda::ExprKind,
) -> Option<(Vec<Symbol>, &mut lambda::ExprKind)> {
    let mut params = Vec::new();
    let body = accumulate_lambda_nodes(expr, &mut params);

    if params.is_empty() {
        None
    } else {
        Some((params, body))
    }
}

pub fn uncurry_program(program: &mut Program) {
    let mut new_lets = vec![];

    for (name, let_) in &mut program.lets {
        if let Some((params, body)) = create_big_lambda(&mut let_.body) {
            let name = Qualified {
                path: name.path.clone(),
                name: Symbol::intern(&format!("{}.uncurried", name.name.get())),
            };

            new_lets.push((
                name.clone(),
                LetDecl {
                    name: name.clone(),
                    body: Box::new(lambda::ExprKind::Lambda(
                        params.clone(),
                        Box::new(body.clone()),
                    )),
                    constants: None,
                    is_in_source_code: false,
                },
            ));

            *body = lambda::ExprKind::Application(
                Box::new(lambda::ExprKind::Function(name.clone())),
                params
                    .into_iter()
                    .map(lambda::ExprKind::Variable)
                    .map(Box::new)
                    .collect(),
            );
        }
    }

    program.lets.extend(new_lets);
}

pub fn uncurry(programs: &mut Vec<Program>) {
    for program in programs {
        uncurry_program(program);
    }
}