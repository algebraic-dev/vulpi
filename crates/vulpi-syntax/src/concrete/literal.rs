use crate::tokens::Token;
use vulpi_location::Spanned;
use vulpi_macros::Show;

#[derive(Show)]
pub struct LitString(Token);

#[derive(Show)]
pub struct LitInteger(Token);

#[derive(Show)]
pub struct LitFloat(Token);

#[derive(Show)]
pub struct LitChar(Token);

#[derive(Show)]
pub struct LitUnit {
    pub left: Token,
    pub right: Token,
}

#[derive(Show)]
pub enum LiteralKind {
    String(Token),
    Integer(Token),
    Float(Token),
    Char(Token),
    Unit(Token),
}

pub type Literal = Spanned<LiteralKind>;
