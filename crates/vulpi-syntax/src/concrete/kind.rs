use vulpi_location::Spanned;

use crate::tokens::Token;

use super::Parenthesis;

pub enum KindKind {
    Star(Token),
    Arrow(Box<Kind>, Token, Box<Kind>),
    Parenthesis(Parenthesis<Box<Kind>>),
}

pub type Kind = Spanned<KindKind>;
