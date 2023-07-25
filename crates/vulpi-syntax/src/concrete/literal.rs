use crate::tokens::Token;
use vulpi_location::Spanned;

pub struct LitString(Token);

pub struct LitInteger(Token);

pub struct LitFloat(Token);

pub struct LitChar(Token);

pub struct LitUnit {
    pub left: Token,
    pub right: Token,
}

pub enum LiteralKind {
    String(Token),
    Integer(Token),
    Float(Token),
    Char(Token),
    Unit(Token),
}

pub type Literal = Spanned<LiteralKind>;
