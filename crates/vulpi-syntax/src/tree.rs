//! This module describes the main tree of the parser. It's flexible enough to handle a lot of
//! errors and be resilient in a certain way.

use std::fmt::{self, Debug, Display};

use vulpi_location::Spanned;

use crate::token::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TreeKind {
    Error,
    Program,
    Import,
    ImportModifier,
    ImportDependencies,
    TypeDecl,
    TypeSynonym,
    TypeProduct,
    TypeSum,
    LetDecl,
    LetPattern,
    LetBody,
    Literal,
    Lambda,
    Application,
    Lower,
    Upper,
    Field,
    RecordCreation,
    RecordUpdate,
    BinaryOperation,
    If,
    Match,
    Annotation,
    Block,
    Let,
    LetDo,
    Do,
    Case,
    Cases,

    PatWild,
    PatId,
    PatConstructor,
    PatLiteral,
    PatAnnotation,
    PatOr,
    PatRecord,
    Pattern,

    LiteralString,
    LiteralInt,

    TypeId,
    TypePoly,
    TypeApplication,
    TypeArrow,
    TypeForall,

    Parenthesis,
    Args,
    Type,

    Alias,

    Path,
    PathEnd,
}

pub enum TokenOrNode<T, N> {
    Token(T),
    Node(N),
}

pub type Node<'a> = TokenOrNode<Spanned<Token<'a>>, Tree<'a>>;

impl<'a> Debug for Node<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Token(arg0) => f.debug_tuple("Token").field(&arg0.data.kind).finish(),
            Self::Node(arg0) => f.debug_tuple("Node").field(arg0).finish(),
        }
    }
}

#[derive(Debug)]
pub enum LabelOrKind {
    Label(&'static str),
    Kind(TreeKind),
}

impl From<&'static str> for LabelOrKind {
    fn from(label: &'static str) -> Self {
        Self::Label(label)
    }
}

impl From<TreeKind> for LabelOrKind {
    fn from(kind: TreeKind) -> Self {
        Self::Kind(kind)
    }
}

#[derive(Debug)]
pub struct Tree<'a> {
    pub kind: LabelOrKind,
    pub children: Vec<Node<'a>>,
}

impl<'a> Tree<'a> {
    pub fn pretty_print(
        &self,
        fmt: &mut fmt::Formatter,
        indent: String,
        last: bool,
    ) -> fmt::Result {
        let indent_now = if last {
            format!("{}└ ", indent)
        } else {
            format!("{}├ ", indent)
        };

        writeln!(
            fmt,
            "{}{}",
            indent_now,
            match self.kind {
                LabelOrKind::Label(label) => format!("{}:", label),
                LabelOrKind::Kind(x) => format!("{:?}", x),
            }
        )?;

        let indent = if last {
            format!("{}   ", indent)
        } else {
            format!("{}│ ", indent)
        };

        for (index, child) in self.children.iter().enumerate() {
            let is_last = index == self.children.len() - 1;

            let indent_now = if is_last {
                format!("{}└ ", indent)
            } else {
                format!("{}├ ", indent)
            };

            match child {
                Node::Token(token) => writeln!(fmt, "{}{:?}", indent_now, token.data.kind)?,
                Node::Node(tree) => tree.pretty_print(fmt, indent.clone(), is_last)?,
            }
        }
        Ok(())
    }
}

impl Display for Tree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(f, "".to_string(), true)
    }
}
