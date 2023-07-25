//! This is the parser of the vulpi language. It takes a stream of tokens and produces a tree of
//! nodes. It's a classical LL(1) parser with a recursive descent and pratt parsing.

use std::ops::Range;

use error::ParserError;
use vulpi_lexer::Lexer;
use vulpi_location::{Byte, FileId, Span, Spanned};
use vulpi_report::{Diagnostic, Report};
use vulpi_syntax::tokens::{Token, TokenData};

pub mod error;

pub type Result<T> = std::result::Result<T, error::ParserError>;

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,

    pub last_pos: Span,

    pub current: Token,
    pub next: Token,

    pub eaten: bool,
    pub file: FileId,

    pub reporter: Report,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>, file: FileId, report: Report) -> Self {
        let current = lexer.bump();
        let next = lexer.bump();

        Self {
            lexer,
            current,
            next,
            last_pos: Span {
                file,
                start: Byte(0),
                end: Byte(0),
            },
            eaten: false,
            file,
            reporter: report,
        }
    }

    pub fn bump(&mut self) -> Token {
        self.eaten = true;

        let mut ret = self.lexer.bump();
        std::mem::swap(&mut self.current, &mut self.next);
        std::mem::swap(&mut ret, &mut self.next);

        self.last_pos = ret.value.range.clone();

        ret
    }

    pub fn peek(&self) -> &Token {
        &self.current
    }

    pub fn expect(&mut self, token: TokenData) -> Result<Token> {
        if self.peek().kind == token {
            Ok(self.bump())
        } else {
            self.unexpected()
        }
    }

    pub fn expect_recover(&mut self, token: TokenData) -> Token {
        if self.peek().kind != token {
            let unexpected_err = self.unexpected_err();
            self.report(unexpected_err);
        }
        self.bump()
    }

    pub fn report(&mut self, err: ParserError) {
        self.reporter.report(Diagnostic::new(err));
        self.bump();
    }

    pub fn expect_or_pop_layout(&mut self, token: TokenData) -> Result<()> {
        if self.peek().kind == token {
            self.bump();
        } else {
            self.lexer.pop_layout();
        }
        Ok(())
    }

    fn unexpected<T>(&mut self) -> Result<T> {
        Err(self.unexpected_err())
    }

    fn unexpected_err(&mut self) -> ParserError {
        error::ParserError::UnexpectedToken(self.peek().clone(), self.peek().value.range.clone())
    }

    pub fn at(&self, token: TokenData) -> bool {
        self.peek().kind == token
    }

    pub fn then(&self, token: TokenData) -> bool {
        self.next.kind == token
    }

    pub fn at_any(&self, tokens: &[TokenData]) -> bool {
        tokens.iter().any(|token| self.at(*token))
    }

    pub fn recover(&mut self, at_any: &[TokenData]) -> Vec<Token> {
        let mut tokens = Vec::new();

        while !self.at_any(at_any) && !self.at(TokenData::Eof) {
            tokens.push(self.bump());
        }

        tokens
    }

    pub fn test<T>(&mut self, fun: impl FnOnce(&mut Self) -> Result<T>) -> Result<Option<T>> {
        self.eaten = false;
        let result = fun(self);

        match result {
            Ok(value) => Ok(Some(value)),
            Err(error) if self.eaten => Err(error),
            Err(_) => Ok(None),
        }
    }

    pub fn span(&self) -> Span {
        self.peek().value.range.clone()
    }

    pub fn kind(&self) -> TokenData {
        self.peek().kind
    }

    pub fn spanned<T>(&mut self, fun: impl FnOnce(&mut Self) -> Result<T>) -> Result<Spanned<T>> {
        let start = self.span();
        let value = fun(self)?;
        let end = self.last_pos.clone();

        Ok(Spanned::new(value, start.mix(end)))
    }

    pub fn sep_by<T>(
        &mut self,
        sep: TokenData,
        mut fun: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<(T, Option<Token>)>> {
        let mut values = Vec::new();

        while let Some(res) = self.test(&mut fun)? {
            let sep = if self.at(sep) {
                Some(self.bump())
            } else {
                None
            };

            let at_end = sep.is_none();

            values.push((res, sep));

            if at_end {
                break;
            }
        }

        if self.at(sep) && !values.is_empty() {
            values.last_mut().unwrap().1 = Some(self.bump());
        }

        Ok(values)
    }

    pub fn multiple<T>(&mut self, mut fun: impl FnMut(&mut Self) -> Result<T>) -> Result<Vec<T>> {
        let mut values = Vec::new();

        while let Some(result) = self.test(&mut fun)? {
            values.push(result);
        }

        Ok(values)
    }

    pub fn with_span(&mut self, start: Range<Byte>) -> Range<Byte> {
        let end = self.last_pos.clone();
        start.start..end.end
    }
}
