use vulpi_syntax::{concrete::top_level::*, tokens::TokenData};

use crate::{Parser, Result};

impl<'a> Parser<'a> {
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

    pub fn explicit_type_binder(&mut self) -> Result<ExplicitTypeBinder> {
        let name = self.lower()?;
        let colon = self.expect(TokenData::Colon)?;
        let kind = self.kind()?;
        Ok(ExplicitTypeBinder { name, colon, kind })
    }

    pub fn type_binder(&mut self) -> Result<TypeBinder> {
        if self.at(TokenData::LowerIdent) {
            let lower = self.lower()?;
            Ok(TypeBinder::Implicit(lower))
        } else {
            Ok(TypeBinder::Explicit(
                self.parenthesis(Self::explicit_type_binder)?,
            ))
        }
    }

    pub fn let_case(&mut self) -> Result<LetCase> {
        let pipe = self.expect(TokenData::Bar)?;
        let arm = self.pattern_arm()?;
        Ok(LetCase { pipe, arm })
    }

    pub fn let_decl(&mut self) -> Result<LetDecl> {
        let visibility = self.visibility()?;
        let let_ = self.expect(TokenData::Let)?;
        let name = self.lower()?;
        let binders = self.many(Self::binder)?;

        let ret = if self.at(TokenData::Colon) {
            let colon = self.bump();
            let effects = if self.at(TokenData::LBrace) {
                Some(self.type_effects()?)
            } else {
                None
            };
            let typ = self.typ()?;
            Some((colon, effects, typ))
        } else {
            None
        };

        let body = if self.at(TokenData::Equal) {
            let eq = self.expect(TokenData::Equal)?;
            let expr = self.expr()?;
            LetMode::Body(eq, expr)
        } else {
            LetMode::Cases(self.many(Self::let_case)?)
        };

        Ok(LetDecl {
            let_,
            name,
            binders,
            body,
            visibility,
            ret,
        })
    }

    pub fn constructor_decl(&mut self) -> Result<Constructor> {
        let pipe = self.expect(TokenData::Bar)?;
        let name = self.upper()?;
        let args = self.many(Self::type_atom)?;
        Ok(Constructor { pipe, name, args })
    }

    pub fn sum_decl(&mut self) -> Result<SumDecl> {
        let constructors = self.many(Self::constructor_decl)?;
        Ok(SumDecl { constructors })
    }

    pub fn field(&mut self) -> Result<Field> {
        let visibility = self.visibility()?;
        let name = self.lower()?;
        let colon = self.expect(TokenData::Colon)?;
        let typ = self.typ()?;
        Ok(Field {
            name,
            colon,
            ty: typ,
            visibility,
        })
    }

    pub fn record_decl(&mut self) -> Result<RecordDecl> {
        let left_brace = self.expect(TokenData::LBrace)?;
        let fields = self.sep_by(TokenData::Comma, Self::field)?;
        let right_brace = self.expect(TokenData::RBrace)?;

        Ok(RecordDecl {
            left_brace,
            fields,
            right_brace,
        })
    }

    pub fn type_def(&mut self) -> Result<TypeDef> {
        match self.token() {
            TokenData::Bar => self.sum_decl().map(TypeDef::Sum),
            TokenData::LBrace => self.record_decl().map(TypeDef::Record),
            _ => self.type_atom().map(TypeDef::Synonym),
        }
    }

    pub fn type_decl(&mut self) -> Result<TypeDecl> {
        let visibility = self.visibility()?;
        let type_ = self.expect(TokenData::Type)?;
        let name = self.upper()?;
        let binders = self.many(Self::type_binder)?;
        let eq = self.expect(TokenData::Equal)?;
        let def = self.type_def()?;

        Ok(TypeDecl {
            type_,
            name,
            binders,
            eq,
            def,
            visibility,
        })
    }

    pub fn use_alias(&mut self) -> Result<UseAlias> {
        let as_ = self.expect(TokenData::As)?;
        let alias = self.upper()?;
        Ok(UseAlias { as_, alias })
    }

    pub fn visibility(&mut self) -> Result<Visibility> {
        if self.at(TokenData::Pub) {
            Ok(Visibility::Public(self.bump()))
        } else {
            Ok(Visibility::Private)
        }
    }

    pub fn use_decl(&mut self) -> Result<UseDecl> {
        let visibility = self.visibility()?;
        let use_ = self.expect(TokenData::Use)?;
        let path = self.path_upper()?;

        let alias = if self.at(TokenData::As) {
            Some(self.use_alias()?)
        } else {
            None
        };

        Ok(UseDecl {
            use_,
            path,
            alias,
            visibility,
        })
    }

    pub fn effect_field(&mut self) -> Result<EffectField> {
        let name = self.lower()?;
        let args = self.many(Self::type_atom)?;
        let colon = self.expect(TokenData::Colon)?;
        let ret = self.typ()?;
        Ok(EffectField {
            name,
            args,
            colon,
            ret,
        })
    }

    pub fn effect_decl(&mut self) -> Result<EffectDecl> {
        let visibility = self.visibility()?;
        let effect = self.expect(TokenData::Effect)?;
        let name = self.upper()?;
        let binders = self.many(Self::type_binder)?;
        let where_ = self.expect(TokenData::Where)?;
        self.expect(TokenData::Begin)?;
        let fields = self.sep_by(TokenData::Sep, Self::effect_field)?;
        self.expect(TokenData::End)?;

        Ok(EffectDecl {
            visibility,
            effect,
            name,
            binders,
            where_,
            fields,
        })
    }

    pub fn top_level(&mut self) -> Result<TopLevel> {
        match self.token() {
            TokenData::Let => self.let_decl().map(Box::new).map(TopLevel::Let),
            TokenData::Type => self.type_decl().map(Box::new).map(TopLevel::Type),
            TokenData::Use => self.use_decl().map(Box::new).map(TopLevel::Use),
            TokenData::Effect => self.effect_decl().map(Box::new).map(TopLevel::Effect),
            _ => self.unexpected(),
        }
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

        let eof = self.eat(TokenData::Eof);
        Program { top_levels, eof }
    }
}
