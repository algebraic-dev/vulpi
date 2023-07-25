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

pub mod error;
mod literals;

use std::{iter::Peekable, str::Chars};

use vulpi_intern::Symbol;
use vulpi_location::{Byte, FileId, Span, Spanned};
use vulpi_report::{Diagnostic, Report};
use vulpi_syntax::tokens::{Comment, Token, TokenData};

/// Checks if a char is a valid identifier part.
pub fn is_identifier_char(char: &char) -> bool {
    char.is_alphanumeric() || matches!(char, |'_'| '!' | '?' | '\'')
}

/// Checks if a char is a whitespace, tab or something like that.
pub fn is_whitespace(char: &char) -> bool {
    matches!(char, '\t' | '\x0C' | '\r' | ' ')
}

/// Checks if a char is a whitespace, tab or something like that.
pub fn is_whitespace_or_line_break(char: &char) -> bool {
    matches!(char, '\n') || is_whitespace(char)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Either<T, U> {
    Left(T),
    Right(U),
}

#[derive(Clone)]
pub enum LexState {
    Common,
    PushLayout,
}
/// A state that can be stored and recovered further in the lexing process.
#[derive(Clone)]
pub struct State {
    index: usize,
    start: usize,
    column: usize,
    line: usize,
    file: FileId,
    layout: Vec<usize>,
    lex_state: LexState,
    reporter: Report,
}

/// The lexer struct that contains the input and the current state. This struct is the entry point
/// for the lexer.
pub struct Lexer<'a> {
    peekable: Peekable<Chars<'a>>,
    input: &'a str,
    state: State,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, file: FileId, reporter: Report) -> Self {
        Self {
            peekable: input.chars().peekable(),
            input,
            state: State {
                index: 0,
                start: 0,
                line: 0,
                file,
                column: 0,
                layout: vec![],
                lex_state: LexState::Common,
                reporter,
            },
        }
    }

    pub fn from(state: State, input: &'a str) -> Self {
        Self {
            peekable: input[state.index..].chars().peekable(),
            input,
            state,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let char = self.peekable.next()?;
        self.state.index += char.len_utf8();

        self.state.column += 1;

        if char == '\n' {
            self.state.column = 0;
            self.state.line += 1;
        }

        Some(char)
    }

    fn save(&mut self) {
        self.state.start = self.state.index;
    }

    fn span(&self) -> Span {
        Span {
            file: self.state.file,
            start: Byte(self.state.start),
            end: Byte(self.state.index),
        }
    }

    fn report(&mut self, message: error::ErrorKind) {
        self.state.reporter.report(Diagnostic::new(error::Error {
            location: self.span(),
            message,
        }));
    }

    fn spanned<T>(&self, token: T) -> Spanned<T> {
        Spanned::new(token, self.span())
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

    fn lex_whitespace(&mut self) -> Spanned<Symbol> {
        self.save();

        self.accumulate(is_whitespace_or_line_break);

        let whitespace = &self.input[self.state.start..self.state.index];
        let whitespace = Symbol::intern(whitespace);

        self.spanned(whitespace)
    }

    fn lex_comment(&mut self) -> Either<Comment, Spanned<Symbol>> {
        let whitespace = self.lex_whitespace();

        self.save();

        let mut cloned = self.peekable.clone();
        cloned.next();

        if let Some(('-', '-')) = self.peekable.peek().zip(cloned.peek()) {
            self.accumulate(|char| *char != '\n');
            let symbol = Symbol::intern(&self.input[self.state.start..self.state.index]);
            let comment = self.spanned(symbol);

            Either::Left(Comment {
                comment,
                whitespace,
            })
        } else {
            Either::Right(whitespace)
        }
    }

    fn lex_comments(&mut self) -> (Vec<Comment>, Spanned<Symbol>) {
        let mut comments = vec![];

        loop {
            match self.lex_comment() {
                Either::Left(comment) => comments.push(comment),
                Either::Right(whitespace) => break (comments, whitespace),
            }
        }
    }

    fn classify_identifier(&mut self) -> TokenData {
        let data = &self.input[self.state.start..self.state.index];
        match data {
            "is" => {
                self.state.lex_state = LexState::PushLayout;
                TokenData::Is
            }
            "do" => {
                self.state.lex_state = LexState::PushLayout;
                TokenData::Do
            }
            "where" => {
                self.state.lex_state = LexState::PushLayout;
                TokenData::Where
            }
            "let" => TokenData::Let,
            "when" => TokenData::When,
            "with" => TokenData::With,
            "if" => TokenData::If,
            "else" => TokenData::Else,
            "then" => TokenData::Then,
            "use" => TokenData::Use,
            "as" => TokenData::As,
            "type" => TokenData::Type,
            "pub" => TokenData::Pub,
            "in" => TokenData::In,
            "forall" => TokenData::Forall,
            "_" => TokenData::Wildcard,
            _ => TokenData::LowerIdent,
        }
    }

    fn classify_token(&mut self, line: usize) -> (TokenData, Symbol) {
        if line != self.state.line {
            let column = self.state.column;
            let last = self.state.layout.last();

            match last {
                None => (),
                Some(last_column) if column > *last_column => (),
                Some(last_column) if column < *last_column => {
                    self.state.layout.pop();
                    return (TokenData::End, Symbol::intern(""));
                }
                Some(_) => return (TokenData::End, Symbol::intern("")),
            }
        }

        let result = if let Some(char) = self.advance() {
            match char {
                '{' => TokenData::LBrace,
                '}' => TokenData::RBrace,
                '(' => {
                    if let Some(')') = self.peekable.peek() {
                        self.advance();
                        TokenData::Unit
                    } else {
                        TokenData::LPar
                    }
                }
                ')' => TokenData::RPar,
                '[' => TokenData::LBracket,
                ']' => TokenData::RBracket,
                '<' => {
                    if let Some('-') = self.peekable.peek() {
                        self.advance();
                        TokenData::LeftArrow
                    } else {
                        TokenData::Less
                    }
                }
                '>' => {
                    if let Some('=') = self.peekable.peek() {
                        self.advance();
                        TokenData::GreaterEqual
                    } else {
                        TokenData::Greater
                    }
                }
                '-' => {
                    if let Some('>') = self.peekable.peek() {
                        self.advance();
                        TokenData::RightArrow
                    } else {
                        TokenData::Minus
                    }
                }
                '+' => TokenData::Plus,
                '*' => TokenData::Star,
                '/' => TokenData::Slash,
                '\\' => TokenData::BackSlash,
                '%' => TokenData::Percent,
                '^' => TokenData::Caret,
                '&' => {
                    if let Some('&') = self.peekable.peek() {
                        self.advance();
                        TokenData::And
                    } else {
                        TokenData::Ampersand
                    }
                }
                '|' => {
                    if let Some('|') = self.peekable.peek() {
                        self.advance();
                        TokenData::Or
                    } else if let Some('>') = self.peekable.peek() {
                        self.advance();
                        TokenData::PipeRight
                    } else {
                        TokenData::Bar
                    }
                }
                '~' => TokenData::Tilde,
                '!' => {
                    if let Some('=') = self.peekable.peek() {
                        self.advance();
                        TokenData::NotEqual
                    } else {
                        TokenData::Exclamation
                    }
                }
                '=' => {
                    if let Some('=') = self.peekable.peek() {
                        self.advance();
                        TokenData::DoubleEqual
                    } else if let Some('>') = self.peekable.peek() {
                        self.advance();
                        TokenData::FatArrow
                    } else {
                        TokenData::Equal
                    }
                }
                ':' => TokenData::Colon,
                ';' => TokenData::Semicolon,
                ',' => TokenData::Comma,
                '.' => TokenData::Dot,
                '0'..='9' => {
                    self.accumulate(|char| char.is_ascii_digit());
                    if let Some('.') = self.peekable.peek() {
                        self.advance();
                        self.accumulate(|char| char.is_ascii_digit());
                        TokenData::Float
                    } else {
                        TokenData::Int
                    }
                }
                '"' => return self.string(),
                'A'..='Z' => {
                    self.accumulate(is_identifier_char);
                    TokenData::UpperIdent
                }
                c if is_identifier_char(&c) => {
                    self.accumulate(is_identifier_char);
                    self.classify_identifier()
                }
                _ => TokenData::Error,
            }
        } else if self.state.layout.pop().is_some() {
            TokenData::End
        } else {
            TokenData::Eof
        };

        let symbol = Symbol::intern(&self.input[self.state.start..self.state.index]);
        (result, symbol)
    }

    pub fn pop_layout(&mut self) {
        self.state.layout.pop();
    }

    /// Lexes a single token from the input.
    pub fn bump(&mut self) -> Token {
        let line = self.state.line;

        let (comments, whitespace) = self.lex_comments();
        self.save();

        let (kind, value) = match self.state.lex_state {
            LexState::Common => self.classify_token(line),

            LexState::PushLayout => {
                self.state.layout.push(self.state.column);
                self.state.lex_state = LexState::Common;
                (TokenData::Begin, Symbol::intern(""))
            }
        };
        Token {
            comments,
            whitespace,
            kind,
            value: self.spanned(value),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.bump())
    }
}

#[cfg(test)]
mod tests {
    use vulpi_report::hash::HashReporter;

    use super::*;

    #[test]
    fn test_lex() {
        let mut lexer = Lexer::new(
            "
            let x = 
                \"a\\\"ta\"
            ",
            FileId(0),
            Report::new(HashReporter::new()),
        );

        let mut token = lexer.bump();

        while token.kind != TokenData::Eof {
            token = lexer.bump();
            println!("{:?} '{}'", token.kind, token.data());
            assert!(token.kind != TokenData::Error);
        }
    }
}
