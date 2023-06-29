//! This module describes the main tree of the parser. It's flexible enough to handle a lot of
//! errors and be resilient in a certain way.

use std::fmt::{self, Debug, Display};

use vulpi_location::Spanned;

use crate::token::{Token, TokenData};

/// Each node in the tree has a kind and a span. The span is used for error reporting and the kind
/// is used for idenfication of the node. It's used as a tag for the node.
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

/// Leaf of a tree that can be either a token or a node.
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

/// This enum express the idea of a Label or a Kind. A [TreeKind] is the kind of the node and a label
/// is just used to identify some nodes in a more granular and easier to find way.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// The green tree that we are using to parse the language. It's a tree that is not aware of the
/// concrete syntax of the language. It's just a tree of nodes that can be either a token or a node.
#[derive(Debug)]
pub struct Tree<'a> {
    pub kind: LabelOrKind,
    pub children: Vec<Node<'a>>,
}

impl<'a> Display for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{}", token.data.data),
            Self::Node(tree) => write!(f, "{}", tree),
        }
    }
}

impl<'a> Tree<'a> {
    /// Traverses the tree and applies a function to each node. It returns a vector of the results.
    pub fn traverse<T>(&self, f: impl FnMut(&Node<'a>) -> Option<T>) -> Option<Vec<T>> {
        self.children.iter().map(f).collect()
    }

    /// Finds a node in the tree by its label.
    pub fn find(&self, label: &str) -> Option<&Tree<'a>> {
        self.children
            .iter()
            .filter_map(|child| match child {
                Node::Node(tree) => Some(tree),
                Node::Token(_) => None,
            })
            .find(|tree| match tree.kind {
                LabelOrKind::Label(l) => l == label,
                _ => false,
            })
    }

    /// Pretty prints the tree. It's used for debugging purposes.
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

pub trait Specialized<'a>
where
    Self: Sized,
{
    const KIND: TreeKind;

    fn tree(&self) -> &'a Tree<'a>;

    fn make(node: &'a Tree<'a>) -> Self;

    fn try_as_tree(tree: &'a Tree<'a>) -> Option<Self> {
        if tree.kind == LabelOrKind::Kind(Self::KIND) {
            Some(Self::make(tree))
        } else {
            None
        }
    }

    fn try_as(node: &'a Node<'a>) -> Option<Self> {
        if let Node::Node(tree) = node {
            Self::try_as_tree(tree)
        } else {
            None
        }
    }

    fn find(&'a self, label: &str) -> Option<&Tree<'a>> {
        self.tree().find(label)
    }

    fn at(&'a self, index: usize) -> Option<&Node<'a>> {
        self.tree().children.get(index)
    }

    fn tag(&'a self) -> Option<LabelOrKind> {
        Some(self.tree().kind)
    }
}

impl<'a> Node<'a> {
    pub fn find(&self, label: &str) -> Option<&Tree<'a>> {
        if let Node::Node(tree) = self {
            tree.find(label)
        } else {
            None
        }
    }

    pub fn children(&'a self) -> Option<&'a Vec<Node<'a>>> {
        if let Node::Node(tree) = self {
            Some(&tree.children)
        } else {
            None
        }
    }

    pub fn lower_id(&'a self) -> Option<String> {
        if let Node::Token(token) = self {
            if let TokenData::LowerIdent = token.data.kind {
                Some(token.data.data.to_string())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn kind(&'a self) -> Option<TreeKind> {
        if let Node::Node(tree) = self {
            match tree.kind {
                LabelOrKind::Kind(kind) => Some(kind),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn to<T: Specialized<'a>>(&'a self) -> Option<T> {
        T::try_as(self)
    }
}

impl<'a> Tree<'a> {
    pub fn to<T: Specialized<'a>>(&'a self) -> Option<T> {
        T::try_as_tree(self)
    }
}
