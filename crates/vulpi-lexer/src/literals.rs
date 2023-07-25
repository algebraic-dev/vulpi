//! Lexing of literals like strings, integers, floats, etc.

use vulpi_intern::Symbol;
use vulpi_syntax::tokens::TokenData;

use crate::{error::ErrorKind, Lexer};

impl<'a> Lexer<'a> {
    /// Parses a character of a char literal
    pub fn char(&mut self) -> Option<char> {
        match self.peekable.peek() {
            Some('\\') => self.escape(),
            Some(_) => self.advance(),
            None => None,
        }
    }

    /// Parses escaped characters
    pub(crate) fn escape(&mut self) -> Option<char> {
        self.advance();

        let result = match self.peekable.peek() {
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('0') => '\0',
            Some('\\') => '\\',
            Some('\'') => '\'',
            Some('"') => '"',
            _ => return None,
        };

        self.advance();

        Some(result)
    }

    pub(crate) fn string(&mut self) -> (TokenData, Symbol) {
        let mut string = String::new();

        while let Some(c) = self.peekable.peek() {
            match c {
                '\\' => {
                    if let Some(res) = self.escape() {
                        string.push(res);
                    } else {
                        self.accumulate(|x| *x != '"');
                        return (TokenData::Error, Symbol::intern(&string));
                    }
                }
                '"' => break,
                _ => {
                    string.push(self.advance().unwrap());
                }
            }
        }

        if let Some('"') = self.peekable.peek() {
            self.advance();
            (TokenData::String, Symbol::intern(&string))
        } else {
            self.report(ErrorKind::UnfinishedString);
            (TokenData::Error, Symbol::intern(&string))
        }
    }
}
