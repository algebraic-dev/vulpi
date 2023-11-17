//! This module declares a bunch of tokens that are units of meaning in the language. There are a
//! bunch of them that are virtual token.

use std::fmt::Debug;

use vulpi_intern::Symbol;
use vulpi_location::Spanned;
use vulpi_show::{Show, TreeDisplay};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenData {
    Let,      // 'let' keyword
    When,     // 'when' keyword
    Is,       // 'is' keyword
    With,     // 'with' keyword
    If,       // 'if' keyword
    Else,     // 'else' keyword
    Then,     // 'then' keyword
    Use,      // 'use' keyword
    As,       // 'as' keyword
    Type,     // 'type' keyword
    Pub,      // 'pub' keyword
    Do,       // 'do' keyword
    In,       // 'in' keyword
    Impl,     // 'impl' keyword
    Trait,    // 'trait' keyword
    Forall,   // 'forall' keyword
    Where,    // 'where' keyword
    Mod,      // 'mod' keyword
    Handle,   // 'handle' keyword
    Cases,    // 'request' keyword
    Effect,   // 'effect' keyword
    External, // 'external' keyword

    String, // String literal
    Int,    // Integer literal
    Float,  // Float Literal
    Char,   // Char literal

    LBrace,     // '{'
    RBrace,     // '}'
    LPar,       // '('
    RPar,       // ')'
    LBracket,   // '['
    RBracket,   // ']'
    LeftArrow,  // '<-'
    RightArrow, // '->'
    FatArrow,   // '=>'
    Unit,

    LowerIdent, // Identifier
    UpperIdent, // Identifier
    Wildcard,

    Colon,       // ':'
    Semicolon,   // ';'
    Comma,       // ','
    Dot,         // '.'
    Exclamation, // '!'
    Equal,       // '='
    Bar,         // '|'
    PipeRight,   // '|>'
    PlusPlus,    // '++'    

    Plus,      // '+'
    Minus,     // '-'
    Star,      // '*'
    Slash,     // '/'
    BackSlash, // '\'
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

    Begin, // Virtual token for beginning of a block
    End,   // Virtual token for end of a block
    Sep,   // Virtual token for a semicolon

    Error,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Comment {
    pub whitespace: Spanned<Symbol>,
    pub comment: Spanned<Symbol>,
}

#[derive(Clone)]
pub struct Token {
    pub comments: Vec<Comment>,
    pub whitespace: Spanned<Symbol>,
    pub kind: TokenData,
    pub value: Spanned<Symbol>,
}

impl Show for Token {
    fn show(&self) -> vulpi_show::TreeDisplay {
        TreeDisplay::label("Token").with(TreeDisplay::label(&self.to_string()))
    }
}

impl Token {
    pub fn is(&self, kind: TokenData) -> bool {
        self.kind == kind
    }

    pub fn data(&self) -> String {
        self.value.data.get()
    }

    pub fn symbol(&self) -> Symbol {
        self.value.data.clone()
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Token").field(&self.kind).finish()
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        use TokenData::*;

        match self.kind {
            String => format!("\"{}\"", self.value.data.get()),
            Int => format!("int({})", self.value.data.get()),
            Float => format!("float({})", self.value.data.get()),
            LowerIdent => format!("lower {}", self.value.data.get()),
            UpperIdent => format!("upper {}", self.value.data.get()),
            Colon => ":".to_string(),
            Semicolon => ";".to_string(),
            Comma => ",".to_string(),
            Dot => ".".to_string(),
            Exclamation => "!".to_string(),
            Equal => "=".to_string(),
            Bar => "|".to_string(),
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
            Begin => "{{".to_string(),
            End => "}}".to_string(),
            Sep => ";".to_string(),
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
            Trait => "trait".to_string(),
            Impl => "impl".to_string(),
            Do => "do".to_string(),
            Where => "where".to_string(),
            Forall => "forall".to_string(),
            In => "in".to_string(),
            LBrace => "{{".to_string(),
            RBrace => "}}".to_string(),
            LPar => "(".to_string(),
            RPar => ")".to_string(),
            LBracket => "[".to_string(),
            RBracket => "]".to_string(),
            LeftArrow => "<-".to_string(),
            RightArrow => "->".to_string(),
            FatArrow => "=>".to_string(),
            BackSlash => "\\".to_string(),
            PipeRight => "|>".to_string(),
            Char => format!("char('{}')", self.value.data.get()),
            Unit => "()".to_string(),
            Wildcard => "_".to_string(),
            Mod => "mod".to_string(),
            Handle => "handle".to_string(),
            Cases => "cases".to_string(),
            Effect => "effect".to_string(),
            External => "external".to_string(),
            PlusPlus => "++".to_string(),
        }
    }
}
