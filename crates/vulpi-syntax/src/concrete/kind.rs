use vulpi_location::Spanned;
use vulpi_macros::Show;

use crate::tokens::Token;

use super::{Parenthesis, Upper};

#[derive(Show)]
pub enum KindType {
    Star(Token),
    Variable(Upper),
    Arrow(Box<Kind>, Token, Box<Kind>),
    Parenthesis(Parenthesis<Box<Kind>>),
}

pub type Kind = Spanned<KindType>;
