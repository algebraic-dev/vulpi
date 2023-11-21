//! Pattern match compilation out of a transformed AST.

use std::collections::HashSet;

use vulpi_intern::Symbol;
use vulpi_macros::Show;
use vulpi_syntax::{
    elaborated::{PatApplication, Pattern, PatternKind},
    lambda::{Case, Index, Occurrence, Tree, Expr},
};

#[derive(Default, Show)]
pub struct Problem {
    matrix: Vec<Row>,
    occurrences: Vec<Occurrence>,
    actions: Vec<usize>,
}

pub fn specialize(ocur: &Occurrence, case: Case) -> Vec<Occurrence> {
    match case {
        Case::Literal(_) => vec![],
        Case::Tuple(size) => (0..size).map(|x| ocur.with(Index::Tuple(x))).collect(),
        Case::Constructor(_, size) => (0..size).map(|x| ocur.with(Index::Cons(x))).collect(),
    }
}

#[derive(Show)]
pub struct Row(Vec<Pattern>);

impl Row {
    pub fn join(&self, before: Row) -> Row {
        let mut row = before.0.clone();
        row.extend(self.shift().0);
        Row(row)
    }

    pub fn shift(&self) -> Row {
        Row(self.0[1..].to_vec())
    }

    pub fn specialize(&self, case: Case) -> Option<Row> {
        use PatternKind::*;

        match (case, *self.0[0].clone()) {
            (_, Error) => unreachable!(),
            (_, Wildcard) => Some(self.shift()),
            (_, Variable(_)) => Some(self.shift()),

            (Case::Literal(l), Literal(r)) if l == r => Some(self.shift()),
            (Case::Constructor(l, _), Application(PatApplication { func, args })) if l == func => {
                Some(self.join(Row(args)))
            }
            (Case::Tuple(x), Tuple(y)) if x == y.len() => Some(self.join(Row(y))),

            _ => None,
        }
    }

    pub fn default(&self) -> Option<Row> {
        use PatternKind::*;

        match *self.0[0] {
            Error => unreachable!(),
            Wildcard | Variable(_) => Some(Row(self.0[1..].to_vec())),
            _ => None,
        }
    }

    pub fn swap(&self, from: usize, to: usize) -> Row {
        let mut row = self.0.clone();
        row.swap(from, to);
        Row(row)
    }

    pub fn is_irrefutable(&self) -> bool {
        self.0
            .iter()
            .all(|x| matches!(&**x, PatternKind::Wildcard | PatternKind::Variable(_)))
    }
}

impl Problem {
    pub fn new(scrutinee: Vec<Expr>, patterns: Vec<Vec<Pattern>>) -> Self {
        let occurrences = scrutinee
            .into_iter()
            .map(|x| Occurrence(x, vec![]))
            .collect::<Vec<_>>();

        let actions = (0..patterns.len()).collect();

        Self {
            matrix: patterns.into_iter().map(Row).collect(),
            occurrences,
            actions,
        }
    }

    pub fn specialize(&self, case: Case) -> Problem {
        let mut problem = Problem::default();

        for (i, row) in self.matrix.iter().enumerate() {
            if let Some(res) = row.specialize(case.clone()) {
                problem.matrix.push(res);
                problem.actions.push(self.actions[i]);
            }
        }

        let mut occurrences = specialize(&self.occurrences[0], case);
        occurrences.extend(self.occurrences.iter().skip(1).cloned());

        problem.occurrences = occurrences;

        problem
    }

    pub fn defaults(&self) -> Problem {
        let mut problem = Problem::default();

        for (i, row) in self.matrix.iter().enumerate() {
            if let Some(res) = row.default() {
                problem.matrix.push(res);
                problem.actions.push(self.actions[i]);
            }
        }

        problem.occurrences = self.occurrences.clone();

        problem
    }

    pub fn swap(&self, from: usize, to: usize) -> Problem {
        let mut problem = Problem::default();

        for row in &self.matrix {
            let row = row.swap(from, to);
            problem.matrix.push(row);
        }

        problem.actions = self.actions.clone();
        problem.occurrences = self.occurrences.clone();

        problem
    }

    pub fn is_refutable(&self, column: usize) -> bool {
        self.matrix.iter().any(|x| {
            matches!(
                &*x.0[column],
                PatternKind::Literal(_) | PatternKind::Application(_) | PatternKind::Tuple(_)
            )
        })
    }

    pub fn head_patterns(&self, column: usize) -> HashSet<Case> {
        let mut heads = HashSet::default();

        for row in &self.matrix {
            match &*row.0[column] {
                PatternKind::Literal(l) => {
                    heads.insert(Case::Literal(l.clone()));
                }
                PatternKind::Application(PatApplication { func, args }) => {
                    heads.insert(Case::Constructor(func.clone(), args.len()));
                }
                PatternKind::Tuple(x) => {
                    heads.insert(Case::Tuple(x.len()));
                }
                _ => (),
            }
        }

        heads
    }

    pub fn find_refutable(&self) -> usize {
        let columns = self.matrix[0].0.len();

        for column in 0..columns {
            if self.is_refutable(column) {
                return column;
            }
        }

        unreachable!("no refutable patterns found")
    }

    pub fn compile(&self) -> Tree {
        if self.matrix.is_empty() {
            Tree::Fail
        } else if self.matrix[0].is_irrefutable() {
            Tree::Leaf(self.actions[0], self.occurrences.clone())
        } else {
            let refutable = self.find_refutable();
            let problem = self.swap(0, refutable);
            let heads = problem.head_patterns(0);

            let mut branches = vec![];

            for head in heads {
                let problem = problem.specialize(head.clone());
                let branch = problem.compile();
                branches.push((head, branch));
            }

            Tree::Switch(self.occurrences[0].clone(), branches)
        }
    }
}

pub fn compile(scrutinee: Vec<Expr>, patterns: Vec<Vec<Pattern>>) -> Tree {
    let problem = Problem::new(scrutinee, patterns);
    problem.compile()
}

pub fn pattern_binders(expr: Expr, pat: &Pattern) -> Vec<(Occurrence, Symbol)> {
    pub fn bind(pattern: &Pattern, ocur: Occurrence, binders: &mut Vec<(Occurrence, Symbol)>) {
        match &**pattern {
            PatternKind::Variable(x) => {
                binders.push((ocur, x.clone()));
            }
            PatternKind::Application(func) => {
                for (i, arg) in func.args.iter().enumerate() {
                    bind(&arg, ocur.with(Index::Cons(i + 1)), binders);
                }
            }
            PatternKind::Tuple(parts) => {
                for (i, part) in parts.into_iter().enumerate() {
                    bind(part, ocur.with(Index::Tuple(i)), binders);
                }
            }
            _ => (),
        }
    }

    let mut binders = vec![];
    bind(pat, Occurrence(expr, vec![]), &mut binders);
    binders
}
