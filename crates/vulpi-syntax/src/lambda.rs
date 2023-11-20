use std::{collections::HashMap, fmt::Display};

use vulpi_intern::Symbol;
use vulpi_location::Span;
use vulpi_macros::Show;

use crate::{elaborated::Literal, r#abstract::Qualified};

#[derive(Clone, Show)]
pub enum Index {
    Cons(usize),
    Tuple(usize),
}

impl Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Index::Cons(s) => write!(f, "c{s}"),
            Index::Tuple(s) => write!(f, "t{s}"),
        }
    }
}

#[derive(Clone)]
pub struct Occurrence(pub Expr, pub Vec<Index>);

impl vulpi_show::Show for Occurrence {
    fn show(&self) -> vulpi_show::TreeDisplay {
        let f = self
            .1
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(".");
        vulpi_show::TreeDisplay::label(&format!("Occurrence(v.{f})"))
    }
}

impl Occurrence {
    pub fn with(&self, index: Index) -> Occurrence {
        let mut indices = self.1.clone();
        indices.push(index);
        Occurrence(self.0.clone(), indices)
    }
}

#[derive(Clone)]
pub enum Tree {
    Fail,
    Leaf(usize, Vec<Occurrence>),
    Switch(Occurrence, Vec<(Case, Tree)>),
}

impl vulpi_show::Show for Tree {
    fn show(&self) -> vulpi_show::TreeDisplay {
        match self {
            Tree::Fail => vulpi_show::TreeDisplay::label("Fail"),
            Tree::Leaf(action, _) => vulpi_show::TreeDisplay::label(&format!("Leaf({})", action)),
            Tree::Switch(_, cases) => {
                let mut tree = vulpi_show::TreeDisplay::label("Switch");

                for (case, rest) in cases {
                    tree = tree.with(
                        vulpi_show::TreeDisplay::label(&format!("case {:?}:", case))
                            .with(rest.show()),
                    );
                }

                tree
            }
        }
    }
}

#[derive(Clone, Show, Debug, PartialEq, Eq, Hash)]
pub enum Case {
    Tuple(usize),
    Constructor(Qualified, usize),
    Literal(Literal),
}

#[derive(Show, Clone)]
pub enum Statement {
    Let(Symbol, Expr),
    Expr(Expr),
    Error,
}

#[derive(Show, Clone)]
pub enum ExprKind {
    Lambda(Symbol, Expr),
    Application(Expr, Vec<Expr>),

    Variable(Symbol),
    Constructor(Qualified, Qualified, Vec<Expr>),
    Function(Qualified),

    Projection(Qualified, Expr),
    Let(Symbol, Expr, Expr),
    Seq(Expr, Expr),
    Literal(Literal),

    RecordInstance(Qualified, Vec<(Symbol, Expr)>),
    RecordUpdate(Qualified, Expr, Vec<(Symbol, Expr)>),

    Access(Occurrence),
    Tuple(Vec<Expr>),

    Switch(Vec<Expr>, Tree, Vec<Expr>),
}

pub type Expr = Box<ExprKind>;

#[derive(Show)]
pub struct LetDecl {
    pub name: Qualified,
    pub binders: Vec<Symbol>,
    pub body: Expr,
    pub constants: Option<HashMap<Qualified, Span>>,
}

#[derive(Show)]
pub enum TypeDecl {
    Abstract,
    Enum(Vec<(Qualified, usize)>),
    Record(Vec<Qualified>),
}

#[derive(Show)]
pub struct ExternalDecl {
    pub name: Qualified,
    pub binding: Symbol,
}

#[derive(Show, Default)]
pub struct Program {
    pub lets: HashMap<Qualified, LetDecl>,
    pub types: HashMap<Qualified, TypeDecl>,
    pub externals: HashMap<Qualified, ExternalDecl>,
}
