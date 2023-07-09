//! This is the parser of the vulpi language. It takes a stream of tokens and produces a tree of
//! nodes. It's a classical LL(1) parser with a recursive descent and pratt parsing.

use std::ops::Range;

use error::ParserError;
use vulpi_location::{Byte, Spanned};
use vulpi_report::{Diagnostic, Report};
use vulpi_storage::id::{self, File, Id};
use vulpi_syntax::concrete::*;
use vulpi_syntax::token::{Token, TokenData};

pub use lexer::Lexer;

pub mod error;
pub mod lexer;

pub type Result<T> = std::result::Result<T, error::ParserError>;

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,

    pub last_pos: Range<Byte>,

    pub current: Spanned<Token>,
    pub next: Spanned<Token>,

    pub eaten: bool,
    pub file: Id<id::File>,

    pub reporter: Report,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>, file: Id<id::File>, report: Report) -> Self {
        let current = lexer.bump();
        let next = lexer.bump();

        Self {
            lexer,
            current,
            next,
            last_pos: Byte(0)..Byte(0),
            eaten: false,
            file,
            reporter: report,
        }
    }

    pub fn bump(&mut self) -> Spanned<Token> {
        self.eaten = true;

        let mut ret = self.lexer.bump();
        std::mem::swap(&mut self.current, &mut self.next);
        std::mem::swap(&mut ret, &mut self.next);

        self.last_pos = ret.range.clone();

        ret
    }

    pub fn peek(&self) -> &Spanned<Token> {
        &self.current
    }

    pub fn expect(&mut self, token: TokenData) -> Result<Spanned<Token>> {
        if self.peek().data.kind == token {
            Ok(self.bump())
        } else {
            self.unexpected()
        }
    }

    pub fn expect_recover(&mut self, token: TokenData) -> Spanned<Token> {
        if self.peek().data.kind != token {
            let unexpected_err = self.unexpected_err();
            self.report(unexpected_err);
        }
        self.bump()
    }

    pub fn expect_or_pop_layout(&mut self, token: TokenData) -> Result<()> {
        if self.peek().data.kind == token {
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
        error::ParserError::UnexpectedToken(
            self.peek().data.clone(),
            self.peek().range.clone(),
            self.file,
        )
    }

    pub fn at(&self, token: TokenData) -> bool {
        self.peek().data.kind == token
    }

    pub fn then(&self, token: TokenData) -> bool {
        self.next.data.kind == token
    }

    pub fn at_any(&self, tokens: &[TokenData]) -> bool {
        tokens.iter().any(|token| self.at(*token))
    }

    pub fn recover(&mut self, at_any: &[TokenData]) -> Vec<Spanned<Token>> {
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

    pub fn span(&self) -> Range<Byte> {
        self.peek().range.clone()
    }

    pub fn kind(&self) -> TokenData {
        self.peek().data.kind
    }

    pub fn spanned<T>(&mut self, fun: impl FnOnce(&mut Self) -> Result<T>) -> Result<Spanned<T>> {
        let start = self.span();
        let value = fun(self)?;
        let end = self.last_pos.clone();

        Ok(Spanned::new(value, start.start.0, end.end.0))
    }

    pub fn sep_by<T>(
        &mut self,
        sep: TokenData,
        mut fun: impl FnMut(&mut Self) -> Result<T>,
    ) -> Result<Vec<(T, Option<Spanned<Token>>)>> {
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

impl Parser<'_> {
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
        self.path(|parser| match parser.peek().data.kind {
            TokenData::UpperIdent | TokenData::LowerIdent => Ok(Ident(parser.bump())),
            _ => parser.unexpected(),
        })
    }

    pub fn path_upper(&mut self) -> Result<Path<Upper>> {
        self.path(|parser| parser.upper())
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

    pub fn parenthesis<T>(
        &mut self,
        parse: impl Fn(&mut Self) -> Result<T>,
    ) -> Result<Parenthesis<T>> {
        let left = self.expect(TokenData::LPar)?;
        let data = parse(self)?;
        let right = self.expect(TokenData::RPar)?;

        Ok(Parenthesis { left, data, right })
    }

    pub fn type_variable(&mut self) -> Result<Lower> {
        let ident = self.expect(TokenData::LowerIdent)?;
        Ok(Lower(ident))
    }

    pub fn type_application(&mut self) -> Result<Box<Type>> {
        let func = self.type_atom()?;

        let mut args = vec![];

        while let Some(right) = self.test(Self::type_atom)? {
            args.push(right);
        }

        if args.is_empty() {
            Ok(func)
        } else {
            let start = func.range.start.clone();
            let end = args.last().unwrap().range.end.clone();

            Ok(Box::new(Spanned {
                range: start..end,
                data: TypeKind::Application(TypeApplication { func, args }),
            }))
        }
    }

    pub fn type_arrow(&mut self) -> Result<Box<Type>> {
        let left = self.type_application()?;

        if self.at(TokenData::RightArrow) {
            let arrow = self.bump();

            let effects = if self.at(TokenData::LBrace) {
                Some(self.type_effects()?)
            } else {
                None
            };

            let right = self.type_arrow()?;

            Ok(Box::new(Spanned {
                range: left.range.start.clone()..right.range.end.clone(),
                data: TypeKind::Arrow(TypeArrow {
                    left,
                    arrow,
                    effects,
                    right,
                }),
            }))
        } else {
            Ok(left)
        }
    }

    pub fn type_atom_raw(&mut self) -> Result<TypeKind> {
        match self.kind() {
            TokenData::LowerIdent => self.type_variable().map(TypeKind::Lower),
            TokenData::UpperIdent => self.path(Self::upper).map(TypeKind::Upper),
            TokenData::Unit => Ok(TypeKind::Unit(self.bump())),
            TokenData::LPar => self.parenthesis(Self::typ).map(TypeKind::Parenthesis),
            _ => self.unexpected(),
        }
    }

    pub fn type_atom(&mut self) -> Result<Box<Type>> {
        self.spanned(Self::type_atom_raw).map(Box::new)
    }

    pub fn type_forall(&mut self) -> Result<TypeForall> {
        let forall = self.expect(TokenData::Forall)?;
        let left = self.multiple(Self::type_variable)?;
        let dot = self.expect(TokenData::Dot)?;
        let right = self.typ()?;

        Ok(TypeForall {
            forall,
            params: left,
            dot,
            body: right,
        })
    }

    pub fn type_effects(&mut self) -> Result<Effects> {
        let left_brace = self.expect(TokenData::LBrace)?;

        let effects = self.sep_by(TokenData::Comma, Self::typ)?;

        let right_brace = self.expect(TokenData::RBrace)?;

        Ok(Effects {
            left_brace,
            right_brace,
            effects,
        })
    }

    pub fn typ(&mut self) -> Result<Box<Type>> {
        match self.kind() {
            TokenData::Forall => self
                .spanned(|x| x.type_forall().map(TypeKind::Forall))
                .map(Box::new),
            _ => self.type_arrow(),
        }
    }

    pub fn literal_kind(&mut self) -> Result<LiteralKind> {
        match self.kind() {
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

    pub fn pattern_atom_kind(&mut self) -> Result<PatternKind> {
        match self.kind() {
            TokenData::Wildcard => Ok(PatternKind::Wildcard(self.bump())),
            TokenData::LowerIdent => self.lower().map(PatternKind::Lower),
            TokenData::UpperIdent => self.path_upper().map(PatternKind::Upper),
            TokenData::LPar => self
                .parenthesis(Self::pattern)
                .map(PatternKind::Parenthesis),
            _ => self.literal().map(PatternKind::Literal),
        }
    }

    pub fn pattern_atom(&mut self) -> Result<Box<Pattern>> {
        self.spanned(Self::pattern_atom_kind).map(Box::new)
    }

    pub fn pattern_application_kind(&mut self) -> Result<PatApplication> {
        let func = self.path_upper()?;
        let args = self.multiple(Self::pattern_atom)?;
        Ok(PatApplication { func, args })
    }

    pub fn pattern_application(&mut self) -> Result<Box<Pattern>> {
        if self.at(TokenData::UpperIdent) {
            self.spanned(|this| {
                let result = this.pattern_application_kind()?;
                if result.args.is_empty() {
                    Ok(PatternKind::Upper(result.func))
                } else {
                    Ok(PatternKind::Application(result))
                }
            })
            .map(Box::new)
        } else {
            self.pattern_atom()
        }
    }

    pub fn pattern(&mut self) -> Result<Box<Pattern>> {
        let left = self.pattern_application()?;
        if self.at(TokenData::Bar) {
            let pipe = self.bump();
            let right = self.pattern()?;
            Ok(Box::new(Spanned {
                range: left.range.start.clone()..right.range.end.clone(),
                data: PatternKind::Or(PatOr { left, pipe, right }),
            }))
        } else {
            Ok(left)
        }
    }

    // Expressions

    pub fn let_sttm(&mut self) -> Result<LetSttm> {
        let let_ = self.expect(TokenData::Let)?;
        let pattern = self.pattern()?;
        let eq = self.expect(TokenData::Equal)?;
        let expr = self.expr()?;
        Ok(LetSttm {
            let_,
            pattern,
            eq,
            expr,
        })
    }

    pub fn statement_kind(&mut self) -> Result<Statement> {
        match self.kind() {
            TokenData::Let => self.let_sttm().map(Statement::Let),
            _ => self.expr().map(Statement::Expr),
        }
    }

    pub fn statement(&mut self) -> Statement {
        match self.statement_kind() {
            Ok(ok) => ok,
            Err(err) => {
                self.report(err);
                let tkns = self.recover(&[TokenData::Sep, TokenData::End]);
                Statement::Error(tkns)
            }
        }
    }

    pub fn block(&mut self) -> Result<Block> {
        self.expect(TokenData::Begin)?;
        let statements = self.sep_by(TokenData::Sep, Self::statement_kind)?;
        self.expect_or_pop_layout(TokenData::End)?;
        Ok(Block { statements })
    }

    pub fn expr_atom_kind(&mut self) -> Result<ExprKind> {
        match self.kind() {
            TokenData::UpperIdent | TokenData::LowerIdent => self.path_ident().map(ExprKind::Ident),
            TokenData::LPar => self.parenthesis(Self::expr).map(ExprKind::Parenthesis),
            _ => self.literal().map(ExprKind::Literal),
        }
    }

    pub fn expr_atom(&mut self) -> Result<Box<Expr>> {
        self.spanned(Self::expr_atom_kind).map(Box::new)
    }

    pub fn expr_application(&mut self) -> Result<Box<Expr>> {
        let func = self.acessor()?;
        let args = self.multiple(Self::acessor)?;
        if args.is_empty() {
            Ok(func)
        } else {
            let range = func.range.start.clone()..args.last().unwrap().range.end.clone();
            Ok(Box::new(Spanned {
                range,
                data: ExprKind::Application(ApplicationExpr { func, args }),
            }))
        }
    }

    pub fn expr_binary(&mut self, precedence: u8) -> Result<Box<Expr>> {
        let mut left = self.expr_application()?;

        while let Some((lower, upper, op)) = self.expr_precedence() {
            if lower < precedence {
                break;
            }

            // Cloned peek inside the expr_precedence
            self.bump();

            let right = self.expr_binary(upper)?;

            let range = left.range.start.clone()..right.range.end.clone();

            left = Box::new(Spanned {
                range,
                data: ExprKind::Binary(BinaryExpr { left, op, right }),
            });
        }

        Ok(left)
    }

    pub fn expr_precedence(&mut self) -> Option<(u8, u8, Operator)> {
        match self.kind() {
            TokenData::Plus => Some((1, 2, Operator::Add(self.peek().clone()))),
            TokenData::Minus => Some((1, 2, Operator::Sub(self.peek().clone()))),
            TokenData::Star => Some((3, 4, Operator::Mul(self.peek().clone()))),
            TokenData::Slash => Some((3, 4, Operator::Div(self.peek().clone()))),
            TokenData::Percent => Some((3, 4, Operator::Rem(self.peek().clone()))),
            TokenData::DoubleEqual => Some((5, 6, Operator::Eq(self.peek().clone()))),
            TokenData::NotEqual => Some((5, 6, Operator::Neq(self.peek().clone()))),
            TokenData::Less => Some((7, 8, Operator::Lt(self.peek().clone()))),
            TokenData::LessEqual => Some((7, 8, Operator::Le(self.peek().clone()))),
            TokenData::Greater => Some((7, 8, Operator::Gt(self.peek().clone()))),
            TokenData::GreaterEqual => Some((7, 8, Operator::Ge(self.peek().clone()))),
            TokenData::Or => Some((9, 1, Operator::Or(self.peek().clone()))),
            TokenData::And => Some((9, 1, Operator::And(self.peek().clone()))),
            _ => None,
        }
    }

    pub fn expr_annotation(&mut self) -> Result<Box<Expr>> {
        let left = self.expr_binary(0)?;
        if self.at(TokenData::Colon) {
            let colon = self.bump();
            let right = self.typ()?;
            Ok(Box::new(Spanned {
                range: left.range.start.clone()..right.range.end.clone(),
                data: ExprKind::Annotation(AnnotationExpr {
                    expr: left,
                    colon,
                    ty: right,
                }),
            }))
        } else {
            Ok(left)
        }
    }

    pub fn expr_do(&mut self) -> Result<Box<Expr>> {
        let do_ = self.expect(TokenData::Do)?;
        let block = self.block()?;
        let range = self.with_span(do_.range.clone());
        Ok(Box::new(Spanned {
            range,
            data: ExprKind::Do(DoExpr { do_, block }),
        }))
    }

    pub fn lambda_expr(&mut self) -> Result<Box<Expr>> {
        let lambda = self.expect(TokenData::BackSlash)?;
        let pattern = self.multiple(Self::pattern)?;
        let arrow = self.expect(TokenData::FatArrow)?;
        let expr = self.expr()?;
        let range = self.with_span(lambda.range.clone());
        Ok(Box::new(Spanned {
            range,
            data: ExprKind::Lambda(LambdaExpr {
                lambda,
                patterns: pattern,
                arrow,
                expr,
            }),
        }))
    }

    pub fn acessor(&mut self) -> Result<Box<Expr>> {
        let left = self.expr_atom()?;
        if self.at(TokenData::Dot) {
            let dot = self.bump();
            let field = self.lower()?;
            let range = self.with_span(left.range.clone());
            Ok(Box::new(Spanned {
                range,
                data: ExprKind::Acessor(AcessorExpr {
                    expr: left,
                    dot,
                    field,
                }),
            }))
        } else {
            Ok(left)
        }
    }

    pub fn let_expr(&mut self) -> Result<Box<Expr>> {
        let let_ = self.expect(TokenData::Let)?;
        let pattern = self.pattern()?;
        let eq = self.expect(TokenData::Equal)?;
        let value = self.expr()?;
        let in_ = self.expect(TokenData::In)?;
        let body = self.expr()?;

        let range = self.with_span(let_.range.clone());

        Ok(Box::new(Spanned {
            range,
            data: ExprKind::Let(LetExpr {
                let_,
                pattern,
                eq,
                body: value,
                in_,
                value: body,
            }),
        }))
    }

    pub fn when_case(&mut self) -> Result<WhenArm> {
        let pattern = self.pattern()?;
        let arrow = self.expect(TokenData::FatArrow)?;
        let expr = self.expr()?;
        Ok(WhenArm {
            pattern,
            arrow,
            expr,
        })
    }

    pub fn when_expr(&mut self) -> Result<Box<Expr>> {
        let when = self.expect(TokenData::When)?;
        let scrutinee = self.expr_atom()?;
        let is = self.expect(TokenData::Is)?;

        self.expect(TokenData::Begin)?;

        let cases = self
            .sep_by(TokenData::Sep, Self::when_case)?
            .into_iter()
            .map(|x| x.0)
            .collect();

        self.expect(TokenData::End)?;

        let range = self.with_span(when.range.clone());

        Ok(Box::new(Spanned {
            range,
            data: ExprKind::When(WhenExpr {
                when,
                scrutinee,
                is,
                arms: cases,
            }),
        }))
    }

    pub fn if_expr(&mut self) -> Result<Box<Expr>> {
        let if_ = self.expect(TokenData::If)?;
        let cond = self.expr()?;
        let then = self.expect(TokenData::Then)?;
        let then_expr = self.expr()?;
        let else_ = self.expect(TokenData::Else)?;
        let else_expr = self.expr()?;

        let range = self.with_span(if_.range.clone());

        Ok(Box::new(Spanned {
            range,
            data: ExprKind::If(IfExpr {
                if_,
                cond,
                then,
                then_expr,
                else_,
                else_expr,
            }),
        }))
    }

    pub fn expr_part(&mut self) -> Result<Box<Expr>> {
        match self.kind() {
            TokenData::BackSlash => self.lambda_expr(),
            TokenData::Let => self.let_expr(),
            TokenData::Do => self.expr_do(),
            TokenData::When => self.when_expr(),
            TokenData::If => self.if_expr(),
            _ => self.expr_annotation(),
        }
    }

    pub fn expr(&mut self) -> Result<Box<Expr>> {
        let left = self.expr_part()?;
        if self.at(TokenData::PipeRight) {
            let pipe_right = self.bump();
            let right = self.expr()?;
            let range = self.with_span(left.range.clone());
            Ok(Box::new(Spanned {
                range,
                data: ExprKind::Binary(BinaryExpr {
                    left,
                    op: Operator::Pipe(pipe_right),
                    right,
                }),
            }))
        } else {
            Ok(left)
        }
    }

    pub fn binder(&mut self) -> Result<Binder> {
        let left_paren = self.expect(TokenData::LPar)?;
        let pattern = self.pattern()?;
        let colon = self.expect(TokenData::Colon)?;
        let typ = self.typ()?;
        let right_paren = self.expect(TokenData::RPar)?;
        Ok(Binder {
            left_paren,
            pattern,
            colon,
            typ,
            right_paren,
        })
    }

    pub fn let_decl(&mut self) -> Result<LetDecl> {
        let let_ = self.expect(TokenData::Let)?;
        let name = self.lower()?;
        let binders = self.multiple(Self::binder)?;

        let typ = if self.at(TokenData::Colon) {
            let colon = self.bump();
            let typ = self.typ()?;
            Some((colon, typ))
        } else {
            None
        };

        let eq = self.expect(TokenData::Equal)?;
        let expr = self.expr()?;

        Ok(LetDecl {
            let_,
            name,
            binders,
            typ,
            eq,
            expr,
        })
    }

    pub fn constructor_decl(&mut self) -> Result<Constructor> {
        let pipe = self.expect(TokenData::Bar)?;
        let name = self.upper()?;
        let args = self.multiple(Self::type_atom)?;
        Ok(Constructor { pipe, name, args })
    }

    pub fn sum_decl(&mut self) -> Result<SumDecl> {
        let constructors = self.multiple(Self::constructor_decl)?;
        Ok(SumDecl { constructors })
    }

    pub fn field(&mut self) -> Result<Field> {
        let name = self.lower()?;
        let colon = self.expect(TokenData::Colon)?;
        let typ = self.typ()?;
        Ok(Field {
            name,
            colon,
            ty: typ,
        })
    }

    pub fn record_decl(&mut self) -> Result<RecordDecl> {
        let left_brace = self.expect(TokenData::LBrace)?;
        let fields = self.multiple(Self::field)?;
        let right_brace = self.expect(TokenData::RBrace)?;

        Ok(RecordDecl {
            left_brace,
            fields,
            right_brace,
        })
    }

    pub fn type_def(&mut self) -> Result<TypeDef> {
        match self.kind() {
            TokenData::Bar => self.sum_decl().map(TypeDef::Sum),
            TokenData::LBrace => self.record_decl().map(TypeDef::Record),
            _ => self.type_atom().map(TypeDef::Synonym),
        }
    }

    pub fn type_decl(&mut self) -> Result<TypeDecl> {
        let type_ = self.expect(TokenData::Type)?;
        let name = self.upper()?;
        let binders = self.multiple(Self::lower)?;
        let eq = self.expect(TokenData::Equal)?;
        let def = self.type_def()?;

        Ok(TypeDecl {
            type_,
            name,
            binders,
            eq,
            def,
        })
    }

    pub fn use_alias(&mut self) -> Result<UseAlias> {
        let as_ = self.expect(TokenData::As)?;
        let alias = self.upper()?;
        Ok(UseAlias { as_, alias })
    }

    pub fn use_decl(&mut self) -> Result<UseDecl> {
        let use_ = self.expect(TokenData::Use)?;
        let path = self.path_upper()?;

        let alias = if self.at(TokenData::As) {
            Some(self.use_alias()?)
        } else {
            None
        };

        Ok(UseDecl { use_, path, alias })
    }

    pub fn top_level(&mut self) -> Result<TopLevel> {
        match self.kind() {
            TokenData::Let => self.let_decl().map(Box::new).map(TopLevel::Let),
            TokenData::Type => self.type_decl().map(Box::new).map(TopLevel::Type),
            TokenData::Use => self.use_decl().map(Box::new).map(TopLevel::Use),
            _ => self.unexpected(),
        }
    }

    pub fn report(&mut self, err: ParserError) {
        self.reporter.report(Diagnostic::new(err));
        self.bump();
    }

    pub fn program(&mut self) -> Program {
        let mut top_levels = vec![];

        while !self.at(TokenData::Eof) {
            match self.top_level() {
                Ok(top_level) => top_levels.push(top_level),
                Err(err) => {
                    self.report(err);
                    let errs = self.recover(&[TokenData::Let, TokenData::Type, TokenData::Use]);
                    top_levels.push(TopLevel::Error(errs))
                }
            }
        }

        let eof = self.expect_recover(TokenData::Eof);
        Program { top_levels, eof }
    }
}

pub fn parse(lexer: Lexer<'_>, file: Id<File>, reporter: Report) -> Program {
    let result = Parser::new(lexer, file, reporter).program();

    result
}
