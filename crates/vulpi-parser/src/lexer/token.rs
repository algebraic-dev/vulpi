//! This module declares a bunch of tokens that are units of meaning in the language. There are a
//! bunch of them that are virtual token

use std::fmt::Display;

use vulpi_location::Spanned;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenData {
    Let,  // 'let' keyword
    When, // 'when' keyword
    Is,   // 'is' keyword
    With, // 'with' keyword
    If,   // 'if' keyword
    Else, // 'else' keyword
    Then, // 'then' keyword
    Use,  // 'use' keyword
    As,   // 'as' keyword
    Type, // 'type' keyword
    Pub,  // 'pub' keyword
    Do,   // 'do' keyword

    String, // String literal
    Int,    // Integer literal
    Float,  // Float Literal

    LBrace,     // '{'
    RBrace,     // '}'
    LParen,     // '('
    RParen,     // ')'
    LBracket,   // '['
    RBracket,   // ']'
    LeftArrow,  // '<-'
    RightArrow, // '->'
    FatArrow,   // '=>'

    Identifier, // Identifier

    Colon,       // ':'
    Semicolon,   // ';'
    Comma,       // ','
    Dot,         // '.'
    Exclamation, // '!'
    Equal,       // '='
    Pipe,        // '|'

    Plus,      // '+'
    Minus,     // '-'
    Star,      // '*'
    Slash,     // '/'
    Percent,   // '%'
    Caret,     // '^'
    Ampersand, // '&'
    Tilde,     // '~'

    Greater,      // '>'
    Less,         // '<'
    GreaterEqual, // '>='
    LessEqual,    // '<='
    NotEqual,     // '!='
    DoubleEqual,  // '=='

    And, // '&&'
    Or,  // '||'

    VBegin, // Virtual token for beginning of a block
    VEnd,   // Virtual token for end of a block
    VSemi,  // Virtual token for a semicolon

    Error,
    Eof,
}

#[derive(Debug)]
pub struct Comment<'a> {
    pub whitespace: Spanned<&'a str>,
    pub comment: Spanned<&'a str>,
}

pub struct Token<'a> {
    pub comments: Vec<Comment<'a>>,
    pub whitespace: Spanned<&'a str>,
    pub kind: TokenData,
    pub data: &'a str,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenData::*;

        let data = match self.kind {
            String => format!("\"{}\"", self.data),
            Int => format!("{}", self.data),
            Float => format!("{}", self.data),
            Identifier => format!("{}", self.data),
            Colon => ":".to_string(),
            Semicolon => ";".to_string(),
            Comma => ",".to_string(),
            Dot => ".".to_string(),
            Exclamation => "!".to_string(),
            Equal => "=".to_string(),
            Pipe => "|".to_string(),
            Plus => "+".to_string(),
            Minus => "-".to_string(),
            Star => "*".to_string(),
            Slash => "/".to_string(),
            Percent => "%".to_string(),
            Caret => "^".to_string(),
            Ampersand => "&".to_string(),
            Tilde => "~".to_string(),
            Greater => ">".to_string(),
            Less => "<".to_string(),
            GreaterEqual => ">=".to_string(),
            LessEqual => "<=".to_string(),
            NotEqual => "!=".to_string(),
            DoubleEqual => "==".to_string(),
            And => "&&".to_string(),
            Or => "||".to_string(),
            VBegin => "{{".to_string(),
            VEnd => "}}".to_string(),
            VSemi => ";".to_string(),
            Error => "error".to_string(),
            Eof => "eof".to_string(),
            Let => "let".to_string(),
            When => "when".to_string(),
            Is => "is".to_string(),
            With => "with".to_string(),
            If => "if".to_string(),
            Else => "else".to_string(),
            Then => "then".to_string(),
            Use => "use".to_string(),
            As => "as".to_string(),
            Type => "type".to_string(),
            Pub => "pub".to_string(),
            Do => "do".to_string(),
            LBrace => "{{".to_string(),
            RBrace => "}}".to_string(),
            LParen => "(".to_string(),
            RParen => ")".to_string(),
            LBracket => "[".to_string(),
            RBracket => "]".to_string(),
            LeftArrow => "<-".to_string(),
            RightArrow => "->".to_string(),
            FatArrow => "=>".to_string(),
        };

        write!(f, "{}", data)
    }
}
