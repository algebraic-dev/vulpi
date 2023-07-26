use crate::{Parser, Result};

use vulpi_location::Spanned;
use vulpi_syntax::{
    concrete::{tree::*, Path, Upper},
    tokens::TokenData,
};

impl<'a> Parser<'a> {
    pub fn record_field(&mut self) -> Result<RecordField> {
        let name = self.lower()?;
        let eq = self.expect(TokenData::Equal)?;
        let expr = self.expr()?;
        Ok(RecordField { name, eq, expr })
    }

    pub fn record_instance(&mut self, name: Path<Upper>) -> Result<RecordInstance> {
        let left_brace = self.expect(TokenData::LBrace)?;
        let fields = self.sep_by(TokenData::Comma, Self::record_field)?;
        let right_brace = self.expect(TokenData::RBrace)?;
        Ok(RecordInstance {
            name,
            left_brace,
            fields,
            right_brace,
        })
    }

    pub fn record_update(&mut self, expr: Box<Expr>) -> Result<RecordUpdate> {
        let left_brace = self.expect(TokenData::LBrace)?;
        let fields = self.sep_by(TokenData::Comma, Self::record_field)?;
        let right_brace = self.expect(TokenData::RBrace)?;
        Ok(RecordUpdate {
            expr,
            left_brace,
            fields,
            right_brace,
        })
    }

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

    pub fn statement_kind(&mut self) -> Result<StatementKind> {
        match self.token() {
            TokenData::Let => self.let_sttm().map(StatementKind::Let),
            _ => self.expr().map(StatementKind::Expr),
        }
    }

    pub fn statement(&mut self) -> Statement {
        match self.spanned(Self::statement_kind) {
            Ok(ok) => ok,
            Err(err) => {
                self.report(err);
                let tkns = self.recover(&[TokenData::Sep, TokenData::End]);
                Statement {
                    range: tkns[0].value.range.clone(),
                    data: StatementKind::Error(tkns),
                }
            }
        }
    }

    pub fn block(&mut self) -> Result<Block> {
        self.expect(TokenData::Begin)?;
        let mut statements = Vec::new();

        while !self.at(TokenData::End) {
            let stmt = self.statement();

            let sep = if self.at(TokenData::Sep) {
                Some(self.bump())
            } else {
                None
            };

            statements.push((stmt, sep));
        }

        self.expect_or_pop_layout(TokenData::End)?;
        Ok(Block { statements })
    }

    pub fn expr_atom_kind(&mut self) -> Result<ExprKind> {
        match self.token() {
            TokenData::UpperIdent | TokenData::LowerIdent => {
                let path = self.path_ident()?;

                if path.last.0.kind == TokenData::UpperIdent && self.at(TokenData::LBrace) {
                    Ok(ExprKind::RecordInstance(self.record_instance(Path {
                        segments: path.segments,
                        last: Upper(path.last.0),
                        span: path.span,
                    })?))
                } else {
                    Ok(ExprKind::Ident(path))
                }
            }
            TokenData::LPar => self.parenthesis(Self::expr).map(ExprKind::Parenthesis),
            _ => self.literal().map(ExprKind::Literal),
        }
    }

    pub fn expr_atom(&mut self) -> Result<Box<Expr>> {
        self.spanned(Self::expr_atom_kind).map(Box::new)
    }

    pub fn expr_application(&mut self) -> Result<Box<Expr>> {
        let func = self.acessor()?;
        let args = self.many(Self::acessor)?;
        if args.is_empty() {
            Ok(func)
        } else {
            let range = func.range.clone().mix(args.last().unwrap().range.clone());
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

            let range = left.range.clone().mix(right.range.clone());

            left = Box::new(Spanned {
                range,
                data: ExprKind::Binary(BinaryExpr { left, op, right }),
            });
        }

        Ok(left)
    }

    pub fn expr_precedence(&mut self) -> Option<(u8, u8, Operator)> {
        match self.token() {
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
                range: left.range.clone().mix(right.range.clone()),
                data: ExprKind::Annotation(AnnotationExpr {
                    expr: left,
                    colon,
                    ty: right,
                }),
            }))
        } else if self.at(TokenData::LBrace) {
            let left_range = left.range.clone();
            let right = self.spanned(|this| this.record_update(left))?;
            Ok(Box::new(Spanned {
                range: left_range.mix(right.range.clone()),
                data: ExprKind::RecordUpdate(right.data),
            }))
        } else {
            Ok(left)
        }
    }

    pub fn expr_do(&mut self) -> Result<Box<Expr>> {
        let do_ = self.expect(TokenData::Do)?;
        let block = self.block()?;
        let range = self.with_span(do_.value.range.clone());
        Ok(Box::new(Spanned {
            range,
            data: ExprKind::Do(DoExpr { do_, block }),
        }))
    }

    pub fn lambda_expr(&mut self) -> Result<Box<Expr>> {
        let lambda = self.expect(TokenData::BackSlash)?;
        let pattern = self.many(Self::pattern)?;
        let arrow = self.expect(TokenData::FatArrow)?;
        let expr = self.expr()?;
        let range = self.with_span(lambda.value.range.clone());
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
                data: ExprKind::Acessor(ProjectionExpr {
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

        let range = self.with_span(let_.value.range.clone());

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

    pub fn when_case(&mut self) -> Result<PatternArm> {
        let patterns = self.sep_by(TokenData::Comma, Self::pattern)?;
        let arrow = self.expect(TokenData::FatArrow)?;
        let expr = self.expr()?;
        let guard = if self.at(TokenData::If) {
            let if_ = self.bump();
            let cond = self.expr()?;
            Some((if_, cond))
        } else {
            None
        };

        Ok(PatternArm {
            patterns,
            arrow,
            expr,
            guard,
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

        let range = self.with_span(when.value.range.clone());

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

        let range = self.with_span(if_.value.range.clone());

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

    pub fn cases_expr(&mut self) -> Result<Box<Expr>> {
        let cases = self.expect(TokenData::Cases)?;
        self.expect(TokenData::Begin)?;
        let arms = self.sep_by(TokenData::Sep, Self::when_case)?;
        self.expect_or_pop_layout(TokenData::End)?;
        let range = self.with_span(cases.value.range.clone());
        Ok(Box::new(Spanned {
            range,
            data: ExprKind::Cases(CasesExpr { cases, arms }),
        }))
    }

    pub fn expr_part(&mut self) -> Result<Box<Expr>> {
        match self.token() {
            TokenData::BackSlash => self.lambda_expr(),
            TokenData::Let => self.let_expr(),
            TokenData::Do => self.expr_do(),
            TokenData::When => self.when_expr(),
            TokenData::If => self.if_expr(),
            TokenData::Cases => self.cases_expr(),
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
}
