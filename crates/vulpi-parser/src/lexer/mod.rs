//! The entry point for the Vulpi Lexer. This module contains the first phase o all of the
//! compilers, the lexing phase. This phase splits a source code into [Token]s that are like "words"
//! that contain no connection to other "words". e.g
//!
//! ```rust
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
//! ```
//! do
//!    a c
//!    b
//! ```
//!
//! Turns into
//!
//! ```
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

use std::{iter::Peekable, str::Chars};

use vulpi_location::Spanned;

use self::token::{Comment, Token, TokenData};

pub mod token;

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
    /// The current index in the source code
    pub index: usize,

    /// The start of the current token selection.
    pub start: usize,

    /// The current column
    pub column: usize,

    /// The current line
    pub line: usize,

    /// The layout parsing stack that stores the columns that are currently open.
    pub layout: Vec<usize>,

    /// The current lexing state. It's useful to make something like a automata with more
    /// things.
    pub lex_state: LexState,
}

/// The lexer struct that contains the input and the current state. This struct is the entry point
/// for the lexer.
pub struct Lexer<'a> {
    pub peekable: Peekable<Chars<'a>>,
    pub input: &'a str,
    pub state: State,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            peekable: input.chars().peekable(),
            input,
            state: State {
                index: 0,
                start: 0,
                line: 0,
                column: 0,
                layout: vec![],
                lex_state: LexState::Common,
            },
        }
    }

    /// Advances one char modifying the storable [State].
    pub fn advance(&mut self) -> Option<char> {
        let char = self.peekable.next()?;
        self.state.index += char.len_utf8();

        self.state.column += 1;

        if char == '\n' {
            self.state.column = 0;
            self.state.line += 1;
        }

        Some(char)
    }

    /// Sets the current index to the start index.
    pub fn save(&mut self) {
        self.state.start = self.state.index;
    }

    /// Creates a new spanned token using the selected part of the code.
    pub fn spanned<T>(&self, token: T) -> Spanned<T> {
        Spanned::new(token, self.state.start, self.state.index)
    }

    /// Accumulates chars while the predicate is true.
    fn accumulate(&mut self, predicate: fn(&char) -> bool) {
        while let Some(char) = self.peekable.peek() {
            if predicate(char) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn lex_whitespace(&mut self) -> Spanned<&'a str> {
        self.save();

        self.accumulate(is_whitespace_or_line_break);

        let whitespace = &self.input[self.state.start..self.state.index];

        self.spanned(whitespace)
    }

    fn lex_comment(&mut self) -> Either<Comment<'a>, Spanned<&'a str>> {
        let whitespace = self.lex_whitespace();

        self.save();

        let mut cloned = self.peekable.clone();
        cloned.next();

        if let Some(('-', '-')) = self.peekable.peek().zip(cloned.peek()) {
            self.accumulate(|char| *char != '\n');
            let comment = self.spanned(&self.input[self.state.start..self.state.index]);

            Either::Left(Comment {
                comment,
                whitespace,
            })
        } else {
            Either::Right(whitespace)
        }
    }

    fn lex_comments(&mut self) -> (Vec<Comment<'a>>, Spanned<&'a str>) {
        let mut comments = vec![];

        loop {
            match self.lex_comment() {
                Either::Left(comment) => comments.push(comment),
                Either::Right(whitespace) => break (comments, whitespace),
            }
        }
    }

    pub fn classify_identifier(&mut self) -> TokenData {
        let data = &self.input[self.state.start..self.state.index];
        match data {
            "let" => TokenData::Let,
            "when" => TokenData::When,
            "is" => TokenData::Is,
            "with" => TokenData::With,
            "if" => TokenData::If,
            "else" => TokenData::Else,
            "then" => TokenData::Then,
            "use" => TokenData::Use,
            "as" => TokenData::As,
            "type" => TokenData::Type,
            "pub" => TokenData::Pub,
            "do" => {
                self.state.lex_state = LexState::PushLayout;
                TokenData::Do
            }
            _ => TokenData::Identifier,
        }
    }

    fn classify_token(&mut self, line: usize) -> TokenData {
        if line != self.state.line {
            let column = self.state.column;
            let last = self.state.layout.last();

            match last {
                None => (),
                Some(last_column) if column > *last_column => (),
                Some(last_column) if column < *last_column => {
                    self.state.layout.pop();
                    return TokenData::VEnd;
                }
                Some(_) => return TokenData::VSemi,
            }
        }

        if let Some(char) = self.advance() {
            match char {
                '{' => TokenData::LBrace,
                '}' => TokenData::RBrace,
                '(' => TokenData::LParen,
                ')' => TokenData::RParen,
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
                '%' => TokenData::Percent,
                '^' => TokenData::Caret,
                '&' => TokenData::Ampersand,
                '|' => TokenData::Pipe,
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
                '"' => {
                    self.accumulate(|char| *char != '"');

                    if let Some('"') = self.peekable.peek() {
                        self.advance();
                        TokenData::String
                    } else {
                        TokenData::Error
                    }
                }
                c if is_identifier_char(&c) => {
                    self.accumulate(is_identifier_char);
                    self.classify_identifier()
                }
                _ => TokenData::Error,
            }
        } else if self.state.layout.pop().is_some() {
            TokenData::VEnd
        } else {
            TokenData::Eof
        }
    }

    /// Lexes a single token from the input.
    pub fn lex(&mut self) -> Spanned<Token<'a>> {
        let line = self.state.line;

        let (comments, whitespace) = self.lex_comments();
        self.save();

        let kind = match self.state.lex_state {
            LexState::Common => self.classify_token(line),

            LexState::PushLayout => {
                self.state.layout.push(self.state.column);
                self.state.lex_state = LexState::Common;
                TokenData::VBegin
            }
        };
        self.spanned(Token {
            comments,
            whitespace,
            kind,
            data: self.input[self.state.start..self.state.index].into(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex() {
        let mut lexer = Lexer::new(
            "
            let x = do
            1
             2
            3 4 (do 3 42
                    5
                    6) 
              5
            5
            ",
        );

        let mut token = lexer.lex();

        while token.data.kind != TokenData::Eof {
            println!("{:?} ~ {:?}", token.data.kind, token.data.data);
            token = lexer.lex();
        }
    }
}
