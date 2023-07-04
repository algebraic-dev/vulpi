//! Wrappers for the parse tree. This is the main interface for the parser. It's flexible enough to
//! handle a lot of errors and be resilient in a certain way.

use crate::{
    token::{Token, TokenData},
    tree::{LowerId, Node, Specialized, TokenOrNode, Tree, TreeKind::*, UpperId},
};

use vulpi_location::Spanned;
use vulpi_macros::new_type;
use vulpi_storage::interner::Symbol;

#[new_type(Path)]
pub struct PathNode<'a>(pub &'a mut Tree);

impl<'a> PathNode<'a> {
    pub fn segments(&'a mut self) -> Option<Vec<LowerId>> {
        self.0.traverse(|seg| seg.lower_id())
    }
}

#[new_type(TypeForall)]
pub struct ForallNode<'a>(pub &'a mut Tree);

impl<'a> ForallNode<'a> {
    pub fn args(&'a mut self) -> Option<Vec<LowerId>> {
        self.find("args")?.traverse(|arg| arg.lower_id())
    }

    pub fn body(&'a mut self) -> Option<TypeNode<'a>> {
        self.find("body")?.to()
    }
}

#[new_type(TypeArrow)]
pub struct TypeArrowNode<'a>(pub &'a mut Tree);

impl<'a> TypeArrowNode<'a> {
    pub fn left(&'a mut self) -> Option<TypeNode<'a>> {
        self.find("left")?.to()
    }

    pub fn right(&'a mut self) -> Option<TypeNode<'a>> {
        self.find("right")?.to()
    }
}

#[new_type(TypeApplication)]
pub struct TypeApplicationNode<'a>(pub &'a mut Tree);

impl<'a> TypeApplicationNode<'a> {
    pub fn left(&'a mut self) -> Option<TypeNode<'a>> {
        self.fst()
    }

    pub fn right(&'a mut self) -> Option<TypeNode<'a>> {
        self.at(1)?.to()
    }
}

#[new_type(TypeId)]
pub struct TypeIdNode<'a>(pub &'a mut Tree);

impl<'a> TypeIdNode<'a> {
    pub fn id(&'a mut self) -> Option<LowerId> {
        self.at(0)?.lower_id()
    }
}

#[new_type(TypePoly)]
pub struct TypePolyNode<'a>(pub &'a mut Tree);

impl<'a> TypePolyNode<'a> {
    pub fn id(&'a mut self) -> Option<LowerId> {
        self.at(0)?.lower_id()
    }

    pub fn body(&'a mut self) -> Option<TypeNode<'a>> {
        self.at(1)?.to()
    }
}

pub enum TypeEnum<'a> {
    Forall(ForallNode<'a>),
    Arrow(TypeArrowNode<'a>),
    Application(TypeApplicationNode<'a>),
    Id(TypeIdNode<'a>),
    Poly(TypePolyNode<'a>),
}

#[new_type(Type)]
pub struct TypeNode<'a>(pub &'a mut Tree);

impl<'a> TypeNode<'a> {
    pub fn forall(&'a mut self) -> Option<ForallNode<'a>> {
        self.fst()
    }

    pub fn type_application(&'a mut self) -> Option<TypeApplicationNode<'a>> {
        self.fst()
    }

    pub fn type_arrow(&'a mut self) -> Option<TypeArrowNode<'a>> {
        self.fst()
    }

    pub fn type_id(&'a mut self) -> Option<TypeIdNode<'a>> {
        self.fst()
    }

    pub fn type_poly(&'a mut self) -> Option<TypePolyNode<'a>> {
        self.fst()
    }

    pub fn to_enum(&'a mut self) -> Option<TypeEnum<'a>> {
        let fst = self.at(0)?;
        match fst.kind()? {
            TypeId => fst.to().map(TypeEnum::Id),
            TypePoly => fst.to().map(TypeEnum::Poly),
            TypeApplication => fst.to().map(TypeEnum::Application),
            TypeArrow => fst.to().map(TypeEnum::Arrow),
            TypeForall => fst.to().map(TypeEnum::Forall),
            _ => None,
        }
    }
}

pub struct StringNode<'a>(pub &'a Spanned<Token>);

impl<'a> StringNode<'a> {
    pub fn string(&'a mut self) -> Option<Symbol> {
        Some(self.0.data.data.clone())
    }
}

pub struct IntNode<'a>(pub &'a Spanned<Token>);

impl<'a> IntNode<'a> {
    pub fn int(&'a mut self) -> Option<i64> {
        self.0.data.data.get().parse().ok()
    }
}

pub struct FloatNode<'a>(pub &'a Spanned<Token>);

impl<'a> FloatNode<'a> {
    pub fn float(&'a mut self) -> Option<f64> {
        self.0.data.data.get().parse().ok()
    }
}

pub enum LiteralEnum<'a> {
    String(StringNode<'a>),
    Int(IntNode<'a>),
    Float(FloatNode<'a>),
}

#[new_type(Literal)]
pub struct LiteralNode<'a>(pub &'a mut Tree);

impl<'a> LiteralNode<'a> {
    pub fn to_enum(&'a mut self) -> Option<LiteralEnum<'a>> {
        match self.at(0)? {
            TokenOrNode::Token(token) => match token.data.kind {
                TokenData::String => Some(LiteralEnum::String(StringNode(token))),
                TokenData::Int => Some(LiteralEnum::Int(IntNode(token))),
                TokenData::Float => Some(LiteralEnum::Float(FloatNode(token))),
                _ => None,
            },
            _ => None,
        }
    }
}

#[new_type(PatWild)]
pub struct PatWildNode<'a>(pub &'a mut Tree);

#[new_type(Identifier)]
pub struct PatIdNode<'a>(pub &'a mut Tree);

impl<'a> PatIdNode<'a> {
    pub fn id(&'a mut self) -> Option<LowerId> {
        self.at(0)?.lower_id()
    }
}

#[new_type(PatLiteral)]
pub struct PatLiteralNode<'a>(pub &'a mut Tree);

impl<'a> PatLiteralNode<'a> {
    pub fn literal(&'a mut self) -> Option<LiteralNode<'a>> {
        self.fst()
    }
}
#[new_type(PatConstructor)]
pub struct PatConstructorNode<'a>(pub &'a mut Tree);

impl<'a> PatConstructorNode<'a> {
    pub fn name(&'a mut self) -> Option<PathNode> {
        self.find("path")?.to()
    }

    pub fn args(&'a mut self) -> Vec<PatNode> {
        self.filter(Node::to)
    }
}

#[new_type(PatAnnotation)]
pub struct PatAnnotationNode<'a>(pub &'a mut Tree);

impl<'a> PatAnnotationNode<'a> {
    pub fn pat(&'a mut self) -> Option<PatNode<'a>> {
        self.fst()
    }

    pub fn typ(&'a mut self) -> Option<TypeNode<'a>> {
        self.at(1)?.to()
    }
}

#[new_type(PatOr)]
pub struct PatOrNode<'a>(pub &'a mut Tree);

impl<'a> PatOrNode<'a> {
    pub fn left(&'a mut self) -> Option<PatNode<'a>> {
        self.fst()
    }

    pub fn right(&'a mut self) -> Option<PatNode<'a>> {
        self.at(1)?.to()
    }
}

pub enum PatEnum<'a> {
    Wild(PatWildNode<'a>),
    Id(PatIdNode<'a>),
    Literal(PatLiteralNode<'a>),
    Constructor(PatConstructorNode<'a>),
    Annotation(PatAnnotationNode<'a>),
    Or(PatOrNode<'a>),
}

#[new_type(Pattern)]
pub struct PatNode<'a>(pub &'a mut Tree);

impl<'a> PatNode<'a> {
    pub fn to_enum(&'a mut self) -> Option<PatEnum<'a>> {
        let fst = self.at(0)?;
        match fst.kind()? {
            PatWild => fst.to().map(PatEnum::Wild),
            Identifier => fst.to().map(PatEnum::Id),
            PatLiteral => fst.to().map(PatEnum::Literal),
            PatConstructor => fst.to().map(PatEnum::Constructor),
            PatAnnotation => fst.to().map(PatEnum::Annotation),
            PatOr => fst.to().map(PatEnum::Or),
            _ => None,
        }
    }
}

#[new_type(Annotation)]
pub struct AnnotationNode<'a>(pub &'a mut Tree);

impl<'a> AnnotationNode<'a> {
    pub fn expr(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("expr")?.to()
    }

    pub fn typ(&'a mut self) -> Option<TypeNode<'a>> {
        self.find("type")?.to()
    }
}

#[new_type(PipeExpr)]
pub struct PipeRight<'a>(pub &'a mut Tree);

impl<'a> PipeRight<'a> {
    pub fn left(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("left")?.to()
    }

    pub fn right(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("right")?.to()
    }
}

#[new_type(If)]
pub struct IfNode<'a>(pub &'a mut Tree);

impl<'a> IfNode<'a> {
    pub fn cond(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("cond")?.to()
    }

    pub fn then(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("then")?.to()
    }

    pub fn else_(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("else")?.to()
    }
}

#[new_type(Let)]
pub struct LetNode<'a>(pub &'a mut Tree);

impl<'a> LetNode<'a> {
    pub fn pat(&'a mut self) -> Option<PatNode<'a>> {
        self.find("pattern")?.to()
    }

    pub fn value(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("value")?.to()
    }

    pub fn body(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("body")?.to()
    }
}

#[new_type(Case)]
pub struct CaseNode<'a>(pub &'a mut Tree);

impl<'a> CaseNode<'a> {
    pub fn pat(&'a mut self) -> Option<PatNode<'a>> {
        self.find("pat")?.fst()
    }

    pub fn body(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("body")?.fst()
    }
}

#[new_type(When)]
pub struct WhenNode<'a>(pub &'a mut Tree);

impl<'a> WhenNode<'a> {
    pub fn cond(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("scrutineer")?.to()
    }

    pub fn cases(&'a mut self) -> Option<Vec<CaseNode<'a>>> {
        Some(self.find("cases")?.filter(Node::to))
    }
}

#[new_type(Do)]
pub struct DoNode<'a>(pub &'a mut Tree);

impl<'a> DoNode<'a> {
    pub fn block(&'a mut self) -> Option<&mut Node> {
        self.find("block")?.at(0)
    }
}

#[new_type(Lambda)]
pub struct LambdaNode<'a>(pub &'a mut Tree);

impl<'a> LambdaNode<'a> {
    pub fn pattern(&'a mut self) -> Option<PatNode<'a>> {
        self.find("pattern").and_then(|p| p.to())
    }

    pub fn body(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("body")?.to()
    }
}

#[new_type(Application)]
pub struct ApplicationNode<'a>(pub &'a mut Tree);

impl<'a> ApplicationNode<'a> {
    pub fn func(&'a mut self) -> Option<ExprNode<'a>> {
        self.find("func")?.to()
    }

    pub fn args(&'a mut self) -> Option<&mut Node> {
        self.find("args")?.at(0)
    }
}

#[new_type(Upper)]
pub struct UpperNode<'a>(pub &'a mut Tree);

impl<'a> UpperNode<'a> {
    pub fn name(&'a mut self) -> Option<PathNode<'a>> {
        self.fst()
    }
}

pub enum ExprEnum<'a> {
    Literal(LiteralNode<'a>),
    Upper(UpperNode<'a>),
    Annotation(AnnotationNode<'a>),
    PipeRight(PipeRight<'a>),
    If(IfNode<'a>),
    Let(LetNode<'a>),
    When(WhenNode<'a>),
    Do(DoNode<'a>),
    Lambda(LambdaNode<'a>),
    Application(ApplicationNode<'a>),
}

#[new_type(Expr)]
pub struct ExprNode<'a>(pub &'a mut Tree);

impl<'a> ExprNode<'a> {
    pub fn to_enum(&'a mut self) -> Option<ExprEnum<'a>> {
        let fst = self.at(0)?;
        let kind = fst.kind()?;
        match kind {
            Literal => fst.to().map(ExprEnum::Literal),
            Upper => fst.to().map(ExprEnum::Upper),
            Annotation => fst.to().map(ExprEnum::Annotation),
            PipeExpr => fst.to().map(ExprEnum::PipeRight),
            If => fst.to().map(ExprEnum::If),
            Let => fst.to().map(ExprEnum::Let),
            When => fst.to().map(ExprEnum::When),
            Do => fst.to().map(ExprEnum::Do),
            Lambda => fst.to().map(ExprEnum::Lambda),
            Application => fst.to().map(ExprEnum::Application),
            _ => None,
        }
    }
}

#[new_type(Binder)]
pub struct BinderNode<'a>(pub &'a mut Tree);

impl<'a> BinderNode<'a> {
    pub fn name(&'a mut self) -> Option<LowerId> {
        self.find("name")?.at(0)?.lower_id()
    }

    pub fn typ(&'a mut self) -> Option<TypeNode<'a>> {
        self.find("type")?.to()
    }
}

#[new_type(LetDecl)]
pub struct LetDeclNode<'a>(pub &'a mut Tree);

impl<'a> LetDeclNode<'a> {
    pub fn name(&mut self) -> Option<LowerId> {
        let sim: &mut Self = unsafe { std::mem::transmute(self) };
        sim.at(0)?.lower_id()
    }

    pub fn args(&'a mut self) -> Option<Vec<BinderNode<'a>>> {
        Some(self.find("args")?.filter(Node::to))
    }

    pub fn type_(&'a mut self) -> Option<TypeNode<'a>> {
        self.find("type")?.to()
    }

    pub fn body(&'a mut self) -> Option<ExprNode<'a>> {
        let res = self.find("body")?.at(0)?;
        res.to()
    }
}
#[new_type(Exposed)]
pub struct ExposedNode<'a>(pub &'a mut Tree);

#[new_type(Use)]
pub struct UseNode<'a>(pub &'a mut Tree);

impl<'a> UseNode<'a> {
    pub fn path(&'a mut self) -> Option<PathNode<'a>> {
        self.find("path")?.fst()
    }

    pub fn alias(&'a mut self) -> Option<PathNode<'a>> {
        self.find("alias")?.fst()
    }

    pub fn exposing(&'a mut self) -> Option<Vec<ExposedNode>> {
        Some(self.find("exposing")?.filter(Node::to))
    }
}

#[new_type(DataConstructor)]
pub struct DataConstructorNode<'a>(pub &'a mut Tree);

impl<'a> DataConstructorNode<'a> {
    pub fn name(&'a mut self) -> Option<UpperId> {
        self.find("name")?.at(0)?.upper_id()
    }

    pub fn args(&'a mut self) -> Option<Vec<TypeNode<'a>>> {
        self.find("args")?.traverse(Node::to)
    }
}

#[new_type(TypeSum)]
pub struct TypeSumNode<'a>(pub &'a mut Tree);

impl<'a> TypeSumNode<'a> {
    pub fn constructors(&'a mut self) -> Vec<DataConstructorNode<'a>> {
        self.filter(Node::to)
    }
}

#[new_type(TypeProduct)]
pub struct TypeProductNode<'a>(pub &'a mut Tree);

impl<'a> TypeProductNode<'a> {
    pub fn fields(&'a mut self) -> Option<Vec<TypeNode<'a>>> {
        todo!()
    }
}

#[new_type(TypeSynonym)]
pub struct TypeSynonymNode<'a>(pub &'a mut Tree);

impl<'a> TypeSynonymNode<'a> {
    pub fn type_(&'a mut self) -> Option<TypeNode<'a>> {
        self.fst()
    }
}

#[derive(Debug)]
pub enum TypeDeclEnum<'a> {
    Sum(TypeSumNode<'a>),
    Product(TypeProductNode<'a>),
    Synonym(TypeNode<'a>),
}

#[new_type(TypeDecl)]
pub struct TypeDeclNode<'a>(pub &'a mut Tree);

impl<'a> TypeDeclNode<'a> {
    pub fn name(&'a mut self) -> Option<UpperId> {
        self.find("name")?.at(0)?.upper_id()
    }

    pub fn args(&'a mut self) -> Option<Vec<LowerId>> {
        self.find("args")?.traverse(Node::lower_id)
    }

    pub fn decl_type(&'a mut self) -> Option<TypeDeclEnum<'a>> {
        let place = self.find("type")?.at(0)?;
        match place.kind()? {
            TypeSynonym => place.to().map(TypeDeclEnum::Synonym),
            TypeSum => place.to().map(TypeDeclEnum::Sum),
            TypeProduct => place.to().map(TypeDeclEnum::Product),
            _ => None,
        }
    }
}

#[new_type(TopLevel)]
pub struct TopLevelNode<'a>(pub &'a mut Tree);

pub enum TopLevelEnum<'a> {
    Use(UseNode<'a>),
    Let(LetDeclNode<'a>),
    Type(TypeDeclNode<'a>),
}

impl<'a> TopLevelNode<'a> {
    pub fn use_decl(&'a mut self) -> Option<UseNode<'a>> {
        self.at(0)?.to()
    }

    pub fn let_decl(&'a mut self) -> Option<LetDeclNode<'a>> {
        self.at(0)?.to()
    }

    pub fn type_decl(&'a mut self) -> Option<TypeDeclNode<'a>> {
        self.at(0)?.to()
    }

    pub fn to_enum(&'a mut self) -> Option<TopLevelEnum> {
        let node = self.at(0)?;
        match node.kind()? {
            LetDecl => node.to().map(TopLevelEnum::Let),
            TypeDecl => node.to().map(TopLevelEnum::Type),
            Use => node.to().map(TopLevelEnum::Use),
            _ => None,
        }
    }
}

#[new_type(Program)]
pub struct ProgramNode<'a>(pub &'a mut Tree);

impl<'a> ProgramNode<'a> {
    pub fn top_levels(&'a mut self) -> Vec<TopLevelNode<'a>> {
        self.filter(Node::to)
    }
}
