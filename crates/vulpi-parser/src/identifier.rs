//! Parses multiple types of identifiers and paths

use vulpi_syntax::{
    concrete::{Ident, Lower, Path, Upper},
    tokens::TokenData,
};

use crate::{Parser, Result};

impl<'a> Parser<'a> {
    /// Parses a path from the current token.
    pub fn path<T>(&mut self, parse: impl Fn(&mut Self) -> Result<T>) -> Result<Path<T>> {
        let start = self.span();
        let mut segments = Vec::new();

        while self.at(TokenData::UpperIdent) && self.then(TokenData::Dot) {
            let ident = self.bump();
            let dot = self.bump();
            segments.push((Upper(ident), dot));
        }

        let last = parse(self)?;

        Ok(Path {
            segments,
            last,
            span: self.with_span(start),
        })
    }

    pub fn path_ident(&mut self) -> Result<Path<Ident>> {
        self.path(|parser| match parser.peek().kind {
            TokenData::LowerIdent => Ok(Ident::Lower(parser.lower()?)),
            TokenData::UpperIdent => Ok(Ident::Upper(parser.upper()?)),
            _ => parser.unexpected(),
        })
    }

    pub fn path_upper(&mut self) -> Result<Path<Upper>> {
        self.path(|parser| parser.upper())
    }

    pub fn path_lower(&mut self) -> Result<Path<Lower>> {
        self.path(|parser| parser.lower())
    }

    pub fn lower(&mut self) -> Result<Lower> {
        // TODO: Handle case error
        let ident = self.expect(TokenData::LowerIdent)?;
        Ok(Lower(ident))
    }

    pub fn upper(&mut self) -> Result<Upper> {
        // TODO: Handle case error
        let ident = self.expect(TokenData::UpperIdent)?;
        Ok(Upper(ident))
    }
}
