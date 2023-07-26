use vulpi_syntax::{
    concrete::tree::{Literal, LiteralKind},
    tokens::TokenData,
};

use crate::{Parser, Result};

impl<'a> Parser<'a> {
    pub fn literal_kind(&mut self) -> Result<LiteralKind> {
        match self.token() {
            TokenData::Int => {
                let int = self.bump();
                Ok(LiteralKind::Integer(int))
            }
            TokenData::Float => {
                let float = self.bump();
                Ok(LiteralKind::Float(float))
            }
            TokenData::String => {
                let string = self.bump();
                Ok(LiteralKind::String(string))
            }
            TokenData::Char => {
                let char = self.bump();
                Ok(LiteralKind::Char(char))
            }
            TokenData::Unit => {
                let unit = self.bump();
                Ok(LiteralKind::Unit(unit))
            }
            _ => self.unexpected(),
        }
    }

    pub fn literal(&mut self) -> Result<Literal> {
        self.spanned(Self::literal_kind)
    }
}
