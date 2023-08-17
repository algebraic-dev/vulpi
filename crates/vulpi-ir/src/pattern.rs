//! Compilation of patterns

use std::{
    collections::{HashSet, VecDeque},
    fmt::Display,
};

use vulpi_intern::Symbol;
use vulpi_macros::Show;
use vulpi_syntax::{
    elaborated::PatternArm,
    elaborated::{Literal, Pattern, PatternKind},
    r#abstract::Qualified,
};
use vulpi_typer::{Real, Type};

use crate::ir::{Expr, ExprKind};

#[derive(PartialEq, Hash, Eq, Clone, Show)]
pub enum Case {
    Tuple(usize),
    Cons(Qualified, usize),
    Literal(Literal),
}

#[derive(Clone)]
pub struct Row(usize, VecDeque<Pattern>);

pub fn head_constructor(pat: &Pattern) -> Option<Case> {
    match &**pat {
        PatternKind::Wildcard => None,
        PatternKind::Variable(_) => None,
        PatternKind::Literal(pat) => Some(Case::Literal(pat.clone())),
        PatternKind::Application(app) => Some(Case::Cons(app.func.clone(), app.args.len())),
        PatternKind::Error => unreachable!(),
        PatternKind::Tuple(pats) => Some(Case::Tuple(pats.len())),
    }
}

impl Row {
    pub fn pop_front(self) -> Self {
        let mut pats = self.1;
        pats.pop_front();
        Row(self.0, pats)
    }

    pub fn preppend(self, patterns: Vec<Pattern>) -> Self {
        let mut pats = self.1;
        let mut vec: VecDeque<_> = patterns.into();
        vec.extend(pats);
        Row(self.0, vec)
    }

    pub fn first(&self) -> Option<&Pattern> {
        self.1.front()
    }

    pub fn inline(self) -> Self {
        match *self.first().unwrap().clone() {
            PatternKind::Wildcard => self.pop_front(),
            PatternKind::Variable(_) => self.pop_front(),
            PatternKind::Literal(_) => self.pop_front(),
            PatternKind::Application(app) => self.preppend(app.args),
            PatternKind::Tuple(app) => self.preppend(app),
            PatternKind::Error => todo!(),
        }
    }

    pub fn specialize(self, case: &Case) -> Option<Row> {
        match (case, &**self.first().unwrap()) {
            (Case::Tuple(n), PatternKind::Tuple(pats)) if *n == pats.len() => Some(self.inline()),
            (Case::Cons(q, _), PatternKind::Application(a)) if a.func == *q => Some(self.inline()),
            (Case::Literal(l), PatternKind::Literal(p)) if l == p => Some(self.inline()),
            _ => None,
        }
    }

    pub fn is_all_wildcards(&self) -> bool {
        self.1
            .iter()
            .all(|p| matches!(&**p, PatternKind::Wildcard | PatternKind::Variable(_)))
    }

    pub fn is_empty(&self) -> bool {
        self.1.is_empty()
    }

    pub fn len(&self) -> usize {
        self.1.len()
    }
}

#[derive(Clone, Show)]
pub enum CaseTree {
    Leaf(usize),
    Fail,
    Select(Vec<(Case, CaseTree)>),
}

impl Display for Case {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Case::Tuple(n) => write!(f, "tuple({})", n),
            Case::Cons(q, n) => write!(f, "cons({}, {})", q.name.get(), n),
            Case::Literal(_) => write!(f, "literal"),
        }
    }
}

impl Display for CaseTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CaseTree::Leaf(n) => write!(f, "{}", n),
            CaseTree::Fail => write!(f, "fail"),
            CaseTree::Select(switches) => {
                writeln!(f, "select {{")?;
                for (case, tree) in switches {
                    writeln!(f, "  {} => {},\n", case, tree)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Clone)]
pub struct Problem {
    pub rows: Vec<Row>,
}

impl Problem {
    pub fn new(arms: &[PatternArm<Type<Real>>]) -> Problem {
        let mut rows = vec![];

        for (i, arm) in arms.iter().enumerate() {
            let mut pats = VecDeque::new();
            pats.extend(arm.patterns.clone());
            rows.push(Row(i, pats));
        }

        Problem { rows }
    }

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    pub fn first(&self) -> &Row {
        self.rows.first().unwrap()
    }

    pub fn specialize(self, case: &Case) -> Problem {
        Problem {
            rows: self
                .rows
                .into_iter()
                .filter_map(|row| row.specialize(case))
                .collect(),
        }
    }

    pub fn head_constructors(&self) -> HashSet<Case> {
        self.rows
            .iter()
            .filter_map(|row| head_constructor(row.first().unwrap()))
            .collect()
    }

    pub fn compile(self) -> CaseTree {
        if self.is_empty() {
            CaseTree::Fail
        } else if self.first().is_all_wildcards() {
            CaseTree::Leaf(self.first().0)
        } else {
            let head_constructors = self.head_constructors();
            let mut switches = vec![];

            for constructor in head_constructors {
                let subproblem = self.clone().specialize(&constructor);
                let subswitch = subproblem.compile();
                switches.push((constructor, subswitch));
            }

            CaseTree::Select(switches)
        }
    }
}

pub fn extract_accesors(pat: &Pattern, scrutinee: Expr) -> Vec<(Symbol, Expr)> {
    pub fn go(pat: &Pattern, scrutinee: Expr, map: &mut Vec<(Symbol, Expr)>) {
        match &**pat {
            PatternKind::Wildcard => (),
            PatternKind::Variable(s) => map.push((s.clone(), scrutinee)),
            PatternKind::Literal(_) => (),
            PatternKind::Application(app) => {
                for (i, arg) in app.args.iter().enumerate() {
                    go(
                        arg,
                        Box::new(ExprKind::Projection(scrutinee.clone(), i)),
                        map,
                    );
                }
            }
            PatternKind::Tuple(args) => {
                for (i, arg) in args.iter().enumerate() {
                    go(
                        arg,
                        Box::new(ExprKind::Projection(scrutinee.clone(), i)),
                        map,
                    );
                }
            }
            PatternKind::Error => unreachable!(),
        }
    }

    let mut map = vec![];
    go(pat, scrutinee, &mut map);
    map
}
