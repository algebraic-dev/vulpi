//! This is the parser of the vulpi language. It takes a stream of tokens and produces a tree of
//! nodes. It's based on https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html.

use lexer::Lexer;

use vulpi_location::Spanned;
use vulpi_syntax::token::{Token, TokenData};
use vulpi_syntax::tree::{LabelOrKind, Node, TokenOrNode, Tree, TreeKind};

use TokenData::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Checkpoint(usize);

pub mod lexer;

pub struct TreeBuilder<'a> {
    parents: Vec<(LabelOrKind, usize)>,
    children: Vec<Node<'a>>,
}

impl<'a> TreeBuilder<'a> {
    pub fn token(&mut self, token: Spanned<Token<'a>>) {
        self.children.push(Node::Token(token));
    }

    pub fn open(&mut self, kind: TreeKind) {
        self.parents
            .push((LabelOrKind::Kind(kind), self.children.len()));
    }

    pub fn label<T: Into<LabelOrKind>>(&mut self, label: T) {
        self.parents.push((label.into(), self.children.len()));
    }

    pub fn close(&mut self) {
        let (kind, size) = self.parents.pop().unwrap();
        let children = self.children.drain(size..).collect();
        self.children.push(Node::Node(Tree { kind, children }));
    }

    pub fn checkpoint(&mut self) -> Checkpoint {
        Checkpoint(self.children.len())
    }

    pub fn rollback<T: Into<LabelOrKind>>(&mut self, checkpoint: Checkpoint, kind: T) {
        self.parents.push((kind.into(), checkpoint.0))
    }

    pub fn finish(mut self) -> Tree<'a> {
        match self.children.pop().unwrap() {
            TokenOrNode::Token(_) => panic!("should be a tree"),
            TokenOrNode::Node(tree) => tree,
        }
    }
}

pub struct Parser<'a> {
    builder: TreeBuilder<'a>,
    lexer: Lexer<'a>,
    peek: Spanned<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);

        Self {
            builder: TreeBuilder {
                parents: Vec::new(),
                children: Vec::new(),
            },
            peek: lexer.lex(),
            lexer,
        }
    }

    fn advance(&mut self) {
        let mut next = self.lexer.next().unwrap();
        std::mem::swap(&mut self.peek, &mut next);
        self.builder.token(next);
    }

    fn error(&mut self) {
        self.builder.open(TreeKind::Error);
        self.advance();
        self.builder.close();
    }

    fn expect(&mut self, kind: TokenData) {
        if self.peek() == kind {
            self.advance();
        } else {
            self.error();
        }
    }

    fn eat(&mut self, kind: TokenData) -> bool {
        if self.peek() == kind {
            self.advance();
            true
        } else {
            self.error();
            false
        }
    }

    fn expect_or_close_layout(&mut self, kind: TokenData) {
        if self.peek() == kind {
            self.advance();
        } else {
            self.lexer.pop_layout();
        }
    }

    fn at(&mut self, kind: TokenData) -> bool {
        self.peek() == kind
    }

    fn at_any(&mut self, kinds: &[TokenData]) -> bool {
        kinds.iter().any(|kind| self.at(*kind))
    }

    fn peek(&mut self) -> TokenData {
        self.peek.data.kind
    }

    fn open(&mut self, kind: TreeKind) {
        self.builder.open(kind);
    }

    fn label(&mut self, label: &'static str) {
        self.builder.label(label);
    }

    fn close(&mut self) {
        self.builder.close();
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.builder.checkpoint()
    }

    fn labeled<T: Into<LabelOrKind>>(&mut self, label: T, fun: impl FnOnce(&mut Self)) {
        self.builder.label(label);
        fun(self);
        self.close()
    }

    fn rollback<T: Into<LabelOrKind>>(&mut self, checkpoint: Checkpoint, kind: T) {
        self.builder.rollback(checkpoint, kind);
    }

    pub fn finish(self) -> Tree<'a> {
        self.builder.finish()
    }
}

impl Parser<'_> {
    const PRIMARY_FIRST: &[TokenData] = &[Int, LowerIdent, UpperIdent, Float, String, LPar];

    pub fn path_part(&mut self, checkpoint: Checkpoint, end: fn(&mut Self)) {
        self.rollback(checkpoint, TreeKind::Path);
        while self.at(Dot) {
            self.advance();
            match self.peek() {
                UpperIdent => self.advance(),
                _ => {
                    end(self);
                    break;
                }
            }
        }
        self.close()
    }

    pub fn pattern_path_end(&mut self) {
        self.error()
    }

    pub fn expr_path_end(&mut self) {
        self.expect(LowerIdent)
    }

    pub fn primary(&mut self, atom: fn(&mut Self), path_end: fn(&mut Self)) {
        match self.peek() {
            LowerIdent => self.labeled(TreeKind::Lower, Self::advance),
            UpperIdent => {
                let checkpoint = self.checkpoint();
                self.labeled(TreeKind::Upper, |this| {
                    this.advance();
                    this.path_part(checkpoint, path_end);
                });
            }
            Int => self.labeled(TreeKind::Literal, Self::advance),
            Float => self.labeled(TreeKind::Literal, Self::advance),
            String => self.labeled(TreeKind::Literal, Self::advance),
            LPar => {
                self.open(TreeKind::Parenthesis);
                self.advance();
                atom(self);
                match self.peek() {
                    RPar => {
                        self.advance();
                    }
                    _ => self.error(),
                }
                self.close();
            }
            _ => self.error(),
        }
    }

    pub fn sep1<T: Into<LabelOrKind>>(&mut self, kind: T, sep: TokenData, expr: fn(&mut Self)) {
        let checkpoint = self.checkpoint();
        expr(self);
        if self.at(sep) {
            self.rollback(checkpoint, kind.into());
            while self.at(sep) {
                self.advance();
                expr(self);
            }
            self.close();
        }
    }

    pub fn pattern(&mut self) {
        self.open(TreeKind::Pattern);
        let checkpoint = self.checkpoint();

        self.pattern_cons();

        while self.peek() == Bar {
            self.rollback(checkpoint, TreeKind::PatOr);
            self.advance();
            self.pattern_cons();
            self.close();
        }
        self.close()
    }

    pub fn pattern_cons(&mut self) {
        if self.at(TokenData::UpperIdent) {
            self.open(TreeKind::PatConstructor);
            self.label("path");
            self.upper_path();
            self.close();

            self.label("tags");
            while self.at_any(Self::PRIMARY_FIRST) {
                self.pattern();
            }

            self.close();
            self.close();
        } else {
            self.pattern_atom()
        }
    }

    pub fn binary(&mut self, kind: TreeKind, fun: fn(&mut Self), at: &[TokenData]) {
        let checkpoint = self.checkpoint();
        fun(self);
        while self.at_any(at) {
            self.rollback(checkpoint, kind);
            self.advance();
            fun(self);
            self.close();
        }
    }

    pub fn infix_binding_power(&mut self) -> Option<(u8, u8)> {
        match self.peek() {
            And | Or => Some((3, 4)),
            GreaterEqual | LessEqual | Greater | Less | DoubleEqual | NotEqual => Some((5, 6)),
            Plus | Minus => Some((7, 8)),
            Star | Slash => Some((9, 10)),
            _ => None,
        }
    }

    pub fn expr_atom(&mut self) {
        self.open(TreeKind::Expr);
        self.primary(Self::expr, Self::expr_path_end);
        self.close();
    }

    pub fn pattern_atom(&mut self) {
        self.open(TreeKind::Pattern);
        self.primary(Self::pattern, Self::pattern_path_end);
        self.close();
    }

    pub fn plus(&mut self) {
        self.binary(
            TreeKind::BinaryOperation,
            |this| this.expr_atom(),
            &[Plus, Minus],
        )
    }

    pub fn if_expr(&mut self) {
        self.open(TreeKind::If);
        self.expect(If);
        self.labeled("condition", Self::expr);
        self.expect(Then);
        self.labeled("then", Self::expr);
        self.expect(Else);
        self.labeled("else", Self::closed_expr);
        self.close();
    }

    pub fn let_expr(&mut self) {
        self.open(TreeKind::Let);
        self.expect(Let);
        self.labeled("pattern", Self::pattern);
        self.expect(Equal);
        self.labeled("value", Self::expr);
        self.expect(In);
        self.labeled("body", Self::closed_expr);
        self.close();
    }

    pub fn when_case(&mut self) {
        self.open(TreeKind::Case);
        self.labeled("pat", Self::pattern);
        self.expect(FatArrow);
        self.labeled("body", Self::expr);
        self.close();
    }

    pub fn cases(&mut self) {
        self.expect(TokenData::Begin);
        self.sep1("cases", TokenData::Sep, Self::when_case);
        self.expect_or_close_layout(TokenData::End);
    }

    pub fn when_expr(&mut self) {
        self.open(TreeKind::When);
        self.expect(When);
        self.labeled("scrutineer", Self::expr);
        self.expect(Is);
        self.cases();
        self.close();
    }

    pub fn var_expr(&mut self) {
        if self.at(TokenData::Let) {
            self.open(TreeKind::LetDo);
            self.expect(Let);
            self.labeled("pattern", Self::pattern);
            self.expect(Equal);
            self.labeled("value", Self::expr);
            self.expect(In);
            self.labeled("body", Self::expr);
        } else {
            self.expr()
        }
    }

    pub fn do_expr(&mut self) {
        self.open(TreeKind::Do);
        self.expect(Do);
        self.expect(Begin);
        self.sep1("block", TokenData::Sep, |x| {
            x.labeled("sttm", Self::var_expr)
        });
        self.expect_or_close_layout(TokenData::End);
        self.close();
    }

    pub fn typ_atom(&mut self) {
        self.open(TreeKind::Type);
        match self.peek() {
            LowerIdent => self.labeled(TreeKind::TypePoly, Self::advance),
            UpperIdent => self.labeled(TreeKind::TypeId, Self::advance),
            LPar => {
                self.open(TreeKind::Parenthesis);
                self.advance();
                self.typ();
                match self.peek() {
                    RPar => {
                        self.advance();
                        self.close();
                    }
                    _ => self.error(),
                }
            }
            _ => self.error(),
        }
        self.close();
    }

    pub fn typ_application(&mut self) {
        let checkpoint = self.checkpoint();
        self.typ_atom();
        let mut first = true;

        while !self.at_any(&[
            TokenData::RPar,
            TokenData::RightArrow,
            TokenData::Eof,
            TokenData::Equal,
            TokenData::Comma,
            TokenData::RBrace,
            TokenData::Let,
            TokenData::Type,
        ]) {
            if !first {
                self.rollback(checkpoint, TreeKind::Type);
            }

            self.rollback(checkpoint, TreeKind::TypeApplication);
            self.typ_atom();
            self.close();

            if !first {
                self.close();
            }

            first = false;
        }
    }

    pub fn typ_arrow(&mut self) {
        let checkpoint = self.checkpoint();
        self.typ_application();
        if self.at(RightArrow) {
            self.rollback(checkpoint, TreeKind::Type);
            self.close();
            self.rollback(checkpoint, "left");
            self.close();
            self.rollback(checkpoint, TreeKind::TypeArrow);
            self.advance();
            self.labeled("right", Self::typ);
            self.close();
        }
    }

    pub fn typ_forall(&mut self) {
        self.open(TreeKind::TypeForall);
        self.expect(Forall);
        self.label("args");
        while self.at(LowerIdent) {
            self.advance()
        }
        self.close();
        self.expect(Dot);
        self.labeled("body", Self::typ);
        self.close()
    }

    pub fn typ(&mut self) {
        if self.at(Forall) {
            self.typ_forall()
        } else {
            self.typ_arrow()
        }
    }

    pub fn field(&mut self) {
        let checkpoint = self.checkpoint();
        let checkpoint_field = self.checkpoint();

        self.expr_atom();

        while self.peek() == Dot {
            self.rollback(checkpoint_field, "expr");
            self.close();
            self.rollback(checkpoint, TreeKind::Field);
            self.advance();
            self.labeled("field", |this| this.expect(TokenData::LowerIdent));
            self.close();
        }
    }

    pub fn call(&mut self) {
        let checkpoint = self.checkpoint();
        self.field();
        while self.at_any(Self::PRIMARY_FIRST) {
            self.rollback(checkpoint, TreeKind::Expr);
            self.rollback(checkpoint, TreeKind::Application);
            self.field();
            self.close();
            self.close();
        }
    }

    pub fn annotation(&mut self, checkpoint: Checkpoint) {
        self.rollback(checkpoint, "expr");
        self.close();
        self.rollback(checkpoint, TreeKind::Annotation);
        self.advance();
        self.labeled("type", Self::typ);
        self.close();
    }

    pub fn pipe_right(&mut self, checkpoint: Checkpoint) {
        self.rollback(checkpoint, "left");
        self.close();
        self.rollback(checkpoint, TreeKind::BinaryOperation);
        self.advance();
        self.labeled("right", Self::typ);
        self.close();
    }

    pub fn lambda(&mut self) {
        self.open(TreeKind::Lambda);
        self.expect(TokenData::BackSlash);
        self.labeled("pattern", Self::pattern);
        self.expect(RightArrow);
        self.labeled("body", Self::expr);
        self.close()
    }

    pub fn closed_expr(&mut self) {
        match self.peek() {
            If => self.labeled(TreeKind::Expr, Self::if_expr),
            Let => self.labeled(TreeKind::Expr, Self::let_expr),
            When => self.labeled(TreeKind::Expr, Self::when_expr),
            Do => self.labeled(TreeKind::Expr, Self::do_expr),
            BackSlash => self.labeled(TreeKind::Expr, Self::lambda),
            _ => self.call(),
        }
    }

    pub fn expr(&mut self) {
        let checkpoint = self.checkpoint();

        self.closed_expr();

        while self.at_any(&[PipeRight, Colon]) {
            match self.peek() {
                Colon => self.annotation(checkpoint),
                PipeRight => self.pipe_right(checkpoint),
                _ => self.error(),
            };
        }
    }

    pub fn upper_path(&mut self) {
        let checkpoint = self.checkpoint();
        self.expect(TokenData::UpperIdent);
        self.path_part(checkpoint, |this| this.expect(TokenData::Star));
    }

    pub fn import_as(&mut self) {
        if let As = self.peek() {
            self.advance();
            self.labeled("alias", Self::upper_path);
        }
    }

    pub fn import_decl(&mut self) {
        self.open(TreeKind::Use);
        self.expect(Use);

        self.labeled("path", Self::upper_path);

        self.import_as();

        if let LPar = self.peek() {
            self.advance();
            self.sep1("exposing", TokenData::Comma, |this| {
                this.open(TreeKind::Exposed);
                match this.peek() {
                    UpperIdent => {
                        this.upper_path();
                    }
                    LowerIdent => {
                        this.expect(LowerIdent);
                    }
                    _ => this.error(),
                }
                this.close();
                this.import_as();
            });

            self.expect(RPar);
        }

        self.close();
    }

    pub fn fn_decl(&mut self) {
        self.open(TreeKind::LetDecl);
        self.expect(Let);
        self.expect(LowerIdent);

        self.labeled("args", |this| {
            while this.at(LPar) {
                this.open(TreeKind::Binder);
                this.advance();
                this.labeled("name", |x| x.expect(LowerIdent));
                if this.eat(Colon) {
                    this.labeled("type", Self::typ);
                }
                this.expect(RPar);
            }
        });

        if self.eat(Colon) {
            self.labeled("type", Self::typ);
        }

        self.expect(Equal);

        self.labeled("body", Self::expr);
        self.close();
    }

    pub fn sum_constructor(&mut self) {
        self.open(TreeKind::DataConstructor);
        self.expect(Bar);
        self.labeled("name", |this| this.expect(UpperIdent));

        self.labeled("args", |this| {
            while !this.at_any(&[
                TokenData::Eof,
                TokenData::Bar,
                TokenData::Let,
                TokenData::Type,
                TokenData::Use,
            ]) {
                this.typ_atom()
            }
        });

        self.close();
    }

    pub fn typ_decl(&mut self) {
        self.open(TreeKind::TypeDecl);
        self.expect(Type);
        self.expect(UpperIdent);

        self.labeled("args", |this| {
            while this.at(LowerIdent) {
                this.advance()
            }
        });

        self.expect(Equal);

        self.label("type");

        if self.at(TokenData::Bar) {
            self.labeled(TreeKind::TypeSum, |this| {
                while this.at(Bar) {
                    this.sum_constructor();
                }
            });
        } else if self.at(TokenData::LBrace) {
            self.advance();

            self.labeled(TreeKind::TypeProduct, |this| {
                while !this.at(RBrace) {
                    this.labeled(TreeKind::Field, |this| {
                        this.open(TreeKind::Field);
                        this.expect(LowerIdent);
                        this.expect(Colon);
                        this.typ();
                        this.close()
                    });
                    if this.at(TokenData::Comma) {
                        this.advance();
                    } else {
                        break;
                    }
                }
            });

            self.expect(RBrace);
        } else {
            self.open(TreeKind::TypeSynonym);
            self.typ();
            self.close();
        }

        self.close();

        self.close();
    }

    pub fn top_level(&mut self) {
        match self.peek() {
            TokenData::Use => self.import_decl(),
            TokenData::Let => self.fn_decl(),
            TokenData::Type => self.typ_decl(),
            _ => self.expr(),
        }
    }

    pub fn root(&mut self) {
        self.open(TreeKind::Program);
        self.top_level();
        self.expect(Eof);
        self.close();
    }
}
