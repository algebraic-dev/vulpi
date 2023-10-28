//! The entry point for the Vulpi Lexer. This module contains the first phase o all of the
//! compilers, the lexing phase. This phase splits a source code into [Token]s that are like "words"
//! that contain no connection to other "words". e.g
//!
//! ```hs
//! // If we had a function called lex that given a source code returns a [Vec<Token>] then:
//! lex("1 + 2")
//! // Would return
//! vec![Token::Num(1), Token::Plus, Token::Num(2)]
//! ```
//!
//! This example does not connect tokens it only classify and return a vector of them. The Vulpi
//! lexer, instead, return [Token]s on demand and work with white spaces in order to make
//! *layout parsing* just like haskell.
//!
//! The layout parsing of Vulpi gets whitespace information and turns this information into a set of
//! virtual braces and semicolons. e.g.
//!
//! ```hs
//! do
//!    a c
//!    b
//! ```
//!
//! Turns into
//!
//! ```hs
//! do { a c; b; }
//! ```
//!
//! Following these rules:
//! - Some words are *layout keywords*, these keywords start blocks of indentation and:
//!   - We start a new layout block emitting a block start and push the current column to the stack
//!     after eating all of the spaces and newlines after the layout keyword.
//! - If it's a newline then we need to run the rule
//!   - We get the last layout from the stack and we compare if the current column is:
//!     - Greater: We just continue to the next rule
//!     - Equal: We emit a semicolon
//!     - Less: We emit a block end
//!
//! 

use std::{iter::Peekable, str::Chars};

use state::State;
use token::{TokenData, Token};
use vulpi_location::{Span, Spanned};

pub mod state;
pub mod token;

pub struct Lexer<'a> {
    state: State,
    code: &'a str,
    peekable: Peekable<Chars<'a>>,
}

fn is_identifier_char(char: &char) -> bool {
    char.is_alphanumeric() || matches!(char, |'_'| '!' | '?' | '\'')
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            state: State::new(),
            code,
            peekable: code.chars().peekable(),
        }
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.peekable.next()?;
        self.state.advance(c);
        Some(c)
    }

    fn peek(&mut self) -> Option<&char> {
        self.peekable.peek()
    }

    fn next(&mut self) -> Option<char> {
        self.peekable.next()
    }

    fn save(&mut self) {
        self.state.save();
    }

    fn span(&self) -> Span {
        Span::new(self.state.start, self.state.index)
    }

    fn spanned(&self) -> Spanned<String> {
        Spanned {
            span: self.span(),
            data: self.code[self.state.start..self.state.index].to_string(),
        }
    }

    fn accumulate(&mut self, predicate: fn(&char) -> bool) {
        while let Some(char) = self.peekable.peek() {
            if predicate(char) {
                self.advance();
            } else {
                break;
            }
        }
    }

    pub fn lex(&mut self) -> Token {
        self.save();

        match self.advance() {
            Some(c) => {
                let result = match c {
                    '=' => {
                        match self.peek() {
                            Some('=') => {
                                self.advance();
                                TokenData::DoubleEqual
                            },
                            _ => TokenData::Equal
                        }
                    },
                    '+' => TokenData::Plus,
                    c if c.is_lowercase() => {
                        self.accumulate(is_identifier_char);
                        TokenData::LowerIdent
                    }
                    _ => panic!("cannot read")
                };

                Token {
                    kind: result,
                    value: self.spanned(),
                }
            },
            None => Token {
                kind: TokenData::Eof,
                value: self.spanned(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex() {
        let mut lexer = Lexer::new("a=+==");
        assert_eq!(lexer.lex().kind, TokenData::LowerIdent);
        assert_eq!(lexer.lex().kind, TokenData::Equal);
        assert_eq!(lexer.lex().kind, TokenData::Plus);
        assert_eq!(lexer.lex().kind, TokenData::DoubleEqual);
        assert_eq!(lexer.lex().kind, TokenData::Eof);
    }
}
