//! This module describes the main tree of the parser. It's flexible enough to handle a lot of
//! errors and be resilient in a certain way.

use std::fmt::{self, Debug, Display};

use vulpi_location::Spanned;
use vulpi_storage::interner::Symbol;

use crate::token::{Token, TokenData};

/// Each node in the tree has a kind and a span. The span is used for error reporting and the kind
/// is used for idenfication of the node. It's used as a tag for the node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TreeKind {
    Error,
    Program,
    Exposed,
    Exposes,
    TopLevel,
    ImportModifier,
    ImportDependencies,
    TypeDeclType,
    TypeDecl,
    TypeSynonym,
    TypeProduct,
    TypeSum,
    LetDecl,
    Binder,
    LetPattern,
    Use,
    PipeExpr,
    LetBody,
    Literal,
    Lambda,
    Application,
    Lower,
    Upper,
    Identifier,
    Field,
    RecordCreation,
    RecordUpdate,
    BinaryOperation,
    If,
    Annotation,
    Block,
    Let,
    LetDo,
    When,
    Do,
    Case,
    Expr,
    Cases,

    PatWild,
    PatId,
    PatConstructor,
    PatLiteral,
    PatAnnotation,
    PatOr,
    PatRecord,
    Pattern,

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

    DataConstructor,
}

#[derive(Clone)]
pub struct LowerId(pub Symbol);

#[derive(Clone)]
pub struct UpperId(pub Symbol);

/// Leaf of a tree that can be either a token or a node.
pub enum TokenOrNode<T, N> {
    Token(T),
    Node(N),
}

pub type Node = TokenOrNode<Spanned<Token>, Tree>;

impl Debug for Node {
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
pub struct Tree {
    pub kind: LabelOrKind,
    pub children: Vec<Node>,
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{}", token.data.data.get()),
            Self::Node(tree) => write!(f, "{}", tree),
        }
    }
}

impl Tree {
    pub fn at(&mut self, place: usize) -> Option<&mut Node> {
        self.children.get_mut(place)
    }

    pub fn fst<'a, T: Specialized<'a>>(&'a mut self) -> Option<T> {
        T::try_as(self.children.get_mut(0)?)
    }

    /// Traverses the tree and applies a function to each node. It returns a vector of the results.
    pub fn traverse<'a, T>(&'a mut self, f: fn(&'a mut Node) -> Option<T>) -> Option<Vec<T>> {
        self.children.iter_mut().map(f).collect()
    }

    pub fn filter<'a, T>(&'a mut self, f: fn(&'a mut Node) -> Option<T>) -> Vec<T> {
        self.children.iter_mut().filter_map(f).collect()
    }

    /// Finds a node in the tree by its label.
    pub fn find(&mut self, label: &str) -> Option<&mut Tree> {
        self.children
            .iter_mut()
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
                Node::Token(token) => writeln!(
                    fmt,
                    "{}{:?} ~ {:?}",
                    indent_now,
                    token.data.kind,
                    token.data.data.get()
                )?,
                Node::Node(tree) => tree.pretty_print(fmt, indent.clone(), is_last)?,
            }
        }
        Ok(())
    }
}

impl Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(f, "".to_string(), true)
    }
}

pub trait Specialized<'a>
where
    Self: Sized,
{
    const KIND: TreeKind;

    fn tree(&'a mut self) -> &'a mut Tree;

    fn make(node: &'a mut Tree) -> Self;

    fn fst<T: Specialized<'a>>(&'a mut self) -> Option<T> {
        self.tree().fst()
    }

    fn try_as_tree(tree: &'a mut Tree) -> Option<Self> {
        if tree.kind == LabelOrKind::Kind(Self::KIND) {
            Some(Self::make(tree))
        } else {
            None
        }
    }

    fn try_as(node: &'a mut Node) -> Option<Self> {
        if let Node::Node(tree) = node {
            Self::try_as_tree(tree)
        } else {
            None
        }
    }

    fn find(&'a mut self, label: &'a str) -> Option<&mut Tree> {
        self.tree().find(label)
    }

    fn filter<T>(&'a mut self, f: fn(&'a mut Node) -> Option<T>) -> Vec<T> {
        self.tree().filter(f)
    }

    fn at(&'a mut self, index: usize) -> Option<&'a mut Node> {
        self.tree().children.get_mut(index)
    }

    fn tag(&'a mut self) -> Option<LabelOrKind> {
        Some(self.tree().kind)
    }
}

impl Node {
    pub fn find(&mut self, label: &str) -> Option<&mut Tree> {
        if let Node::Node(tree) = self {
            tree.find(label)
        } else {
            None
        }
    }

    pub fn fst<'a, T: Specialized<'a>>(&'a mut self) -> Option<T> {
        if let Node::Node(tree) = self {
            tree.fst()
        } else {
            None
        }
    }

    pub fn traverse<T>(&mut self, f: fn(&mut Node) -> Option<T>) -> Option<Vec<T>> {
        if let Node::Node(tree) = self {
            tree.traverse(f)
        } else {
            None
        }
    }

    pub fn filter<'a, T>(&'a mut self, f: fn(&'a mut Node) -> Option<T>) -> Vec<T> {
        if let Node::Node(tree) = self {
            tree.filter(f)
        } else {
            Vec::new()
        }
    }

    pub fn children(&self) -> Option<&Vec<Node>> {
        if let Node::Node(tree) = self {
            Some(&tree.children)
        } else {
            None
        }
    }

    pub fn token<T>(&self, fun: fn(&Token) -> Option<T>) -> Option<T> {
        if let Node::Token(token) = self {
            fun(&token.data)
        } else {
            None
        }
    }

    pub fn lower_id(&mut self) -> Option<LowerId> {
        self.token(|token| match token.kind {
            TokenData::LowerIdent => Some(LowerId(token.data.clone())),
            _ => None,
        })
    }

    pub fn upper_id(&self) -> Option<UpperId> {
        self.token(|token| match token.kind {
            TokenData::UpperIdent => Some(UpperId(token.data.clone())),
            _ => None,
        })
    }

    pub fn ident(&self) -> Option<Symbol> {
        self.token(|token| match token.kind {
            TokenData::LowerIdent | TokenData::UpperIdent => Some(token.data.clone()),
            _ => None,
        })
    }

    pub fn string(&self) -> Option<Symbol> {
        if let Node::Token(token) = self {
            if let TokenData::String = token.data.kind {
                Some(token.data.data.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn int(&self) -> Option<i64> {
        if let Node::Token(token) = self {
            if let TokenData::Int = token.data.kind {
                Some(token.data.data.get().parse::<i64>().unwrap())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn float(&self) -> Option<f64> {
        if let Node::Token(token) = self {
            if let TokenData::Float = token.data.kind {
                Some(token.data.data.get().parse::<f64>().unwrap())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn kind(&self) -> Option<TreeKind> {
        if let Node::Node(tree) = self {
            match tree.kind {
                LabelOrKind::Kind(kind) => Some(kind),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn to<'a, T: Specialized<'a>>(&'a mut self) -> Option<T> {
        T::try_as(self)
    }
}

impl Tree {
    pub fn to<'a, T: Specialized<'a>>(&'a mut self) -> Option<T> {
        T::try_as_tree(self)
    }
}
