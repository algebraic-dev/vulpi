use crate::tokens::Token;
use vulpi_location::Spanned;
use vulpi_macros::Show;

#[derive(Show, Clone)]
pub enum LiteralKind {
    String(Token),
    Integer(Token),
    Float(Token),
    Char(Token),
    Unit(Token),
}

pub type Literal = Spanned<LiteralKind>;
