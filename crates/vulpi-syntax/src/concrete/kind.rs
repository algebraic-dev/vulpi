use vulpi_location::Spanned;
use vulpi_macros::Show;

use crate::tokens::Token;

use super::Parenthesis;

#[derive(Show)]
pub enum KindKind {
    Star(Token),
    Arrow(Box<Kind>, Token, Box<Kind>),
    Parenthesis(Parenthesis<Box<Kind>>),
}

pub type Kind = Spanned<KindKind>;
