use vulpi_location::Spanned;
use vulpi_syntax::concrete::{
    r#type::*,
    tree::{Kind, KindType},
    Lower,
};
use vulpi_syntax::tokens::TokenData;

use crate::{Parser, Result};

impl<'a> Parser<'a> {
    fn kind_atom_raw(&mut self) -> Result<KindType> {
        match self.token() {
            TokenData::Star => Ok(KindType::Star(self.bump())),
            TokenData::LPar => Ok(KindType::Parenthesis(self.parenthesis(Self::kind)?)),
            _ => self.unexpected(),
        }
    }

    fn kind_atom(&mut self) -> Result<Box<Kind>> {
        self.spanned(Self::kind_atom_raw).map(Box::new)
    }

    fn kind_arrow(&mut self) -> Result<Box<Kind>> {
        let left = self.kind_atom()?;

        if self.at(TokenData::RightArrow) {
            let arrow = self.bump();
            let right = self.kind()?;

            Ok(Box::new(Spanned {
                span: left.span.clone().mix(right.span.clone()),
                data: KindType::Arrow(left, arrow, right),
            }))
        } else {
            Ok(left)
        }
    }

    pub fn kind(&mut self) -> Result<Box<Kind>> {
        self.kind_arrow()
    }

    fn type_variable(&mut self) -> Result<Lower> {
        self.lower()
    }

    fn type_forall(&mut self) -> Result<TypeForall> {
        let forall = self.expect(TokenData::Forall)?;
        let left = self.many(Self::type_binder)?;
        let dot = self.expect(TokenData::Dot)?;
        let right = self.typ()?;

        Ok(TypeForall {
            forall,
            params: left,
            dot,
            body: right,
        })
    }

    pub fn effect(&mut self) -> Result<Effect> {
        if self.at(TokenData::LowerIdent) {
            let var = self.lower()?;
            Ok(Effect::Variable(var))
        } else {
            let var = self.path_upper()?;
            let args = self.many(Self::type_atom)?;

            Ok(Effect::Application(var, args))
        }
    }

    pub fn type_effects(&mut self) -> Result<Effects> {
        let left_brace = self.expect(TokenData::LBrace)?;
        let effects = self.sep_by(TokenData::Comma, Self::effect)?;
        let right_brace = self.expect(TokenData::RBrace)?;

        Ok(Effects {
            left_brace,
            right_brace,
            effects,
        })
    }

    fn type_atom_raw(&mut self) -> Result<TypeKind> {
        match self.token() {
            TokenData::LowerIdent => self.type_variable().map(TypeKind::TypeVariable),
            TokenData::UpperIdent => self.path(Self::upper).map(TypeKind::Type),
            TokenData::Unit => Ok(TypeKind::Unit(self.bump())),
            TokenData::LPar => {
                let exprs = self.parenthesis(|this| this.sep_by(TokenData::Comma, Self::typ))?;

                if exprs.data.is_empty() {
                    todo!()
                } else if exprs.data.len() == 1 {
                    Ok(TypeKind::Parenthesis(
                        exprs.map(|x| x.into_iter().next().unwrap()),
                    ))
                } else {
                    Ok(TypeKind::Tuple(exprs))
                }
            }

            _ => self.unexpected(),
        }
    }

    pub fn type_atom(&mut self) -> Result<Box<Type>> {
        self.spanned(Self::type_atom_raw).map(Box::new)
    }

    fn type_application(&mut self) -> Result<Box<Type>> {
        let func = self.type_atom()?;

        let args = self.many(Self::type_atom)?;

        if args.is_empty() {
            Ok(func)
        } else {
            let start = func.span.clone();
            let end = args.last().unwrap().span.clone();

            Ok(Box::new(Spanned {
                span: start.mix(end),
                data: TypeKind::Application(TypeApplication { func, args }),
            }))
        }
    }

    fn type_arrow(&mut self) -> Result<Box<Type>> {
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
                span: left.span.clone().mix(right.span.clone()),
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

    /// Parses types
    pub fn typ(&mut self) -> Result<Box<Type>> {
        match self.token() {
            TokenData::Forall => self
                .spanned(|x| x.type_forall().map(TypeKind::Forall))
                .map(Box::new),
            _ => self.type_arrow(),
        }
    }
}
