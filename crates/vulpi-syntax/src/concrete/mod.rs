//! Wrappers for the parse tree. This is the main interface for the parser. It's flexible enough to
//! handle a lot of errors and be resilient in a certain way.

use crate::{
    token::{Token, TokenData},
    tree::{LowerId, Node, Specialized, TokenOrNode, Tree, TreeKind::*, UpperId},
};

use vulpi_location::Spanned;
use vulpi_macros::new_type;
use vulpi_storage::interner::Symbol;

#[derive(Debug)]
pub struct PathNode<'a>(pub &'a Tree);

impl<'a> crate::tree::Specialized<'a> for PathNode<'a> {
    const KIND: crate::tree::TreeKind = Path;
    fn tree(&'a self) -> &'a Tree {
        self.0
    }
    fn make(node: &'a Tree) -> Self {
        Self(node)
    }
}

impl<'a> PathNode<'a> {
    pub fn segments(&'a self) -> Option<Vec<LowerId>> {
        self.0.traverse(|seg| seg.lower_id())
    }
}

/*
#[new_type(TypeForall)]
pub struct ForallNode<'a>(pub &'a Tree);

impl<'a> ForallNode<'a> {
    pub fn args(&'a self) -> Option<Vec<LowerId>> {
        self.find("args")?.traverse(|arg| arg.lower_id())
    }

    pub fn body(&'a self) -> Option<TypeNode<'a>> {
        self.find("body")?.to()
    }
}

#[new_type(TypeArrow)]
pub struct TypeArrowNode<'a>(pub &'a Tree);

impl<'a> TypeArrowNode<'a> {
    pub fn left(&'a self) -> Option<TypeNode<'a>> {
        self.find("left")?.to()
    }

    pub fn right(&'a self) -> Option<TypeNode<'a>> {
        self.find("right")?.to()
    }
}

#[new_type(TypeApplication)]
pub struct TypeApplicationNode<'a>(pub &'a Tree);

impl<'a> TypeApplicationNode<'a> {
    pub fn left(&'a self) -> Option<TypeNode<'a>> {
        self.fst()
    }

    pub fn right(&'a self) -> Option<TypeNode<'a>> {
        self.at(1)?.to()
    }
}

#[new_type(TypeId)]
pub struct TypeIdNode<'a>(pub &'a Tree);

impl<'a> TypeIdNode<'a> {
    pub fn id(&'a self) -> Option<LowerId> {
        self.at(0)?.lower_id()
    }
}

#[new_type(TypePoly)]
pub struct TypePolyNode<'a>(pub &'a Tree);

impl<'a> TypePolyNode<'a> {
    pub fn id(&'a self) -> Option<LowerId> {
        self.at(0)?.lower_id()
    }

    pub fn body(&'a self) -> Option<TypeNode<'a>> {
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
pub struct TypeNode<'a>(pub &'a Tree);

impl<'a> TypeNode<'a> {
    pub fn forall(&'a self) -> Option<ForallNode<'a>> {
        self.fst()
    }

    pub fn type_application(&'a self) -> Option<TypeApplicationNode<'a>> {
        self.fst()
    }

    pub fn type_arrow(&'a self) -> Option<TypeArrowNode<'a>> {
        self.fst()
    }

    pub fn type_id(&'a self) -> Option<TypeIdNode<'a>> {
        self.fst()
    }

    pub fn type_poly(&'a self) -> Option<TypePolyNode<'a>> {
        self.fst()
    }

    pub fn to_enum(&'a self) -> Option<TypeEnum<'a>> {
        match self.at(0)?.kind()? {
            TypeId => self.type_id().map(TypeEnum::Id),
            TypePoly => self.type_poly().map(TypeEnum::Poly),
            TypeApplication => self.type_application().map(TypeEnum::Application),
            TypeArrow => self.type_arrow().map(TypeEnum::Arrow),
            TypeForall => self.forall().map(TypeEnum::Forall),
            _ => None,
        }
    }
}
unwrap()
unwrap()
pub struct StringNode<'a>(pub &'a Spanned<Token>);

impl<'a> StringNode<'a> {
    pub fn string(&'a self) -> Option<Symbol> {
        Some(self.0.data.data.clone())
    }
}

pub struct IntNode<'a>(pub &'a Spanned<Token>);

impl<'a> IntNode<'a> {
    pub fn int(&'a self) -> Option<i64> {
        self.0.data.data.get().parse().ok()
    }
}

pub struct FloatNode<'a>(pub &'a Spanned<Token>);

impl<'a> FloatNode<'a> {
    pub fn float(&'a self) -> Option<f64> {
        self.0.data.data.get().parse().ok()
    }
}

pub enum LiteralEnum<'a> {
    String(StringNode<'a>),
    Int(IntNode<'a>),
    Float(FloatNode<'a>),
}

#[new_type(Literal)]
pub struct LiteralNode<'a>(pub &'a Tree);

impl<'a> LiteralNode<'a> {
    pub fn to_enum(&'a self) -> Option<LiteralEnum<'a>> {
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
pub struct PatWildNode<'a>(pub &'a Tree);

#[new_type(Identifier)]
pub struct PatIdNode<'a>(pub &'a Tree);

impl PatIdNode<'_> {
    pub fn id(&self) -> Option<LowerId> {
        self.at(0)?.lower_id()
    }
}

#[new_type(PatLiteral)]
pub struct PatLiteralNode<'a>(pub &'a Tree);

impl PatLiteralNode<'_> {
    pub fn literal(&self) -> Option<LiteralNode<'_>> {
        self.fst()
    }
}
#[new_type(PatConstructor)]
pub struct PatConstructorNode<'a>(pub &'a Tree);

impl PatConstructorNode<'_> {
    pub fn name(&self) -> Option<PathNode> {
        self.find("path")?.to()
    }

    pub fn args(&self) -> Vec<PatNode> {
        self.filter(|arg| arg.to())
    }
}

#[new_type(PatAnnotation)]
pub struct PatAnnotationNode<'a>(pub &'a Tree);

impl PatAnnotationNode<'_> {
    pub fn pat(&self) -> Option<PatNode<'_>> {
        self.fst()
    }

    pub fn typ(&self) -> Option<TypeNode<'_>> {
        self.at(1)?.to()
    }
}

#[new_type(PatOr)]
pub struct PatOrNode<'a>(pub &'a Tree);

impl PatOrNode<'_> {
    pub fn left(&self) -> Option<PatNode<'_>> {
        self.fst()
    }

    pub fn right(&self) -> Option<PatNode<'_>> {
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
pub struct PatNode<'a>(pub &'a Tree);

impl<'a> PatNode<'a> {
    pub fn to_enum(&'a self) -> Option<PatEnum<'a>> {
        match self.at(0)?.kind()? {
            PatWild => self.fst().map(PatEnum::Wild),
            Identifier => self.fst().map(PatEnum::Id),
            PatLiteral => self.fst().map(PatEnum::Literal),
            PatConstructor => self.fst().map(PatEnum::Constructor),
            PatAnnotation => self.fst().map(PatEnum::Annotation),
            PatOr => self.fst().map(PatEnum::Or),
            _ => None,
        }
    }
}

#[new_type(Annotation)]
pub struct AnnotationNode<'a>(pub &'a Tree);

impl<'a> AnnotationNode<'a> {
    pub fn expr(&'a self) -> Option<ExprNode<'a>> {
        self.find("expr")?.to()
    }

    pub fn typ(&'a self) -> Option<TypeNode<'a>> {
        self.find("type")?.to()
    }
}

#[new_type(PipeExpr)]
pub struct PipeRight<'a>(pub &'a Tree);

impl<'a> PipeRight<'a> {
    pub fn left(&'a self) -> Option<ExprNode<'a>> {
        self.find("left")?.to()
    }

    pub fn right(&'a self) -> Option<ExprNode<'a>> {
        self.find("right")?.to()
    }
}

#[new_type(If)]
pub struct IfNode<'a>(pub &'a Tree);

impl<'a> IfNode<'a> {
    pub fn cond(&'a self) -> Option<ExprNode<'a>> {
        self.find("cond")?.to()
    }

    pub fn then(&'a self) -> Option<ExprNode<'a>> {
        self.find("then")?.to()
    }

    pub fn else_(&'a self) -> Option<ExprNode<'a>> {
        self.find("else")?.to()
    }
}

#[new_type(Let)]
pub struct LetNode<'a>(pub &'a Tree);

impl<'a> LetNode<'a> {
    pub fn pat(&'a self) -> Option<PatNode<'a>> {
        self.find("pattern")?.to()
    }

    pub fn value(&'a self) -> Option<ExprNode<'a>> {
        self.find("value")?.to()
    }

    pub fn body(&'a self) -> Option<ExprNode<'a>> {
        self.find("body")?.to()
    }
}

#[new_type(Case)]
pub struct CaseNode<'a>(pub &'a Tree);

impl<'a> CaseNode<'a> {
    pub fn pat(&'a self) -> Option<PatNode<'a>> {
        self.find("pat")?.fst()
    }

    pub fn body(&'a self) -> Option<ExprNode<'a>> {
        self.find("body")?.fst()
    }
}

#[new_type(When)]
pub struct WhenNode<'a>(pub &'a Tree);

impl<'a> WhenNode<'a> {
    pub fn cond(&'a self) -> Option<ExprNode<'a>> {
        self.find("scrutineer")?.to()
    }

    pub fn cases(&'a self) -> Option<Vec<CaseNode<'a>>> {
        Some(self.find("cases")?.filter(Node::to))
    }
}

#[new_type(Do)]
pub struct DoNode<'a>(pub &'a Tree);

impl<'a> DoNode<'a> {
    pub fn block(&'a self) -> Option<&Node> {
        self.find("block")?.at(0)
    }
}

#[new_type(Lambda)]
pub struct LambdaNode<'a>(pub &'a Tree);

impl<'a> LambdaNode<'a> {
    pub fn pattern(&'a self) -> Option<PatNode<'a>> {
        self.find("pattern").and_then(|p| p.to())
    }

    pub fn body(&'a self) -> Option<ExprNode<'a>> {
        self.find("body")?.to()
    }
}

#[new_type(Application)]
pub struct ApplicationNode<'a>(pub &'a Tree);

impl<'a> ApplicationNode<'a> {
    pub fn func(&self) -> Option<ExprNode<'_>> {
        self.find("func")?.to()
    }

    pub fn args(&self) -> Option<&Node> {
        self.find("args")?.at(0)
    }
}

#[new_type(Upper)]
pub struct UpperNode<'a>(pub &'a Tree);

impl<'a> UpperNode<'a> {
    pub fn name(&'a self) -> Option<PathNode<'a>> {
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
pub struct ExprNode<'a>(pub &'a Tree);

impl ExprNode<'_> {
    pub fn to_enum(&self) -> Option<ExprEnum<'_>> {
        match self.at(0)?.kind()? {
            Literal => self.fst().map(ExprEnum::Literal),
            Upper => self.fst().map(ExprEnum::Upper),
            Annotation => self.fst().map(ExprEnum::Annotation),
            PipeExpr => self.fst().map(ExprEnum::PipeRight),
            If => self.fst().map(ExprEnum::If),
            Let => self.fst().map(ExprEnum::Let),
            When => self.fst().map(ExprEnum::When),
            Do => self.fst().map(ExprEnum::Do),
            Lambda => self.fst().map(ExprEnum::Lambda),
            Application => self.fst().map(ExprEnum::Application),
            _ => None,
        }
    }
}

#[new_type(Binder)]
pub struct BinderNode<'a>(pub &'a Tree);

impl BinderNode<'_> {
    pub fn name(&self) -> Option<LowerId> {
        self.find("name")?.at(0)?.lower_id()
    }

    pub fn typ(&self) -> Option<TypeNode<'_>> {
        self.find("type")?.to()
    }
}

#[new_type(LetDecl)]
pub struct LetDeclNode<'a>(pub &'a Tree);

impl LetDeclNode<'_> {
    pub fn name(&self) -> Option<LowerId> {
        self.at(1)?.lower_id()
    }

    pub fn args(&self) -> Option<Vec<BinderNode<'_>>> {
        self.find("args")?.traverse(Node::to)
    }

    pub fn type_(&self) -> Option<TypeNode<'_>> {
        self.find("type")?.to()
    }

    pub fn body(&self) -> Option<ExprNode<'_>> {
        let res = self.find("body")?.at(0)?;
        res.to()
    }
}
#[new_type(Exposed)]
pub struct ExposedNode<'a>(pub &'a Tree);

#[new_type(Use)]
pub struct UseNode<'a>(pub &'a Tree);

impl UseNode<'_> {
    pub fn path(&self) -> Option<PathNode<'_>> {
        self.find("path")?.fst()
    }

    pub fn alias(&self) -> Option<PathNode<'_>> {
        self.find("alias")?.fst()
    }

    pub fn exposing(&self) -> Option<Vec<ExposedNode>> {
        Some(self.find("exposing")?.filter(Node::to))
    }
}

#[new_type(DataConstructor)]
pub struct DataConstructorNode<'a>(pub &'a Tree);

impl DataConstructorNode<'_> {
    pub fn name(&self) -> Option<UpperId> {
        self.find("name")?.at(0)?.upper_id()
    }

    pub fn args(&self) -> Option<Vec<TypeNode<'_>>> {
        self.find("args")?.traverse(Node::to)
    }
}

#[new_type(TypeSum)]
pub struct TypeSumNode<'a>(pub &'a Tree);

impl TypeSumNode<'_> {
    pub fn constructors(&self) -> Vec<DataConstructorNode<'_>> {
        self.filter(Node::to)
    }
}

#[new_type(TypeProduct)]
pub struct TypeProductNode<'a>(pub &'a Tree);

impl TypeProductNode<'_> {
    pub fn fields(&self) -> Option<Vec<TypeNode<'_>>> {
        todo!()
    }
}

#[new_type(TypeSynonym)]
pub struct TypeSynonymNode<'a>(pub &'a Tree);

impl TypeSynonymNode<'_> {
    pub fn type_(&self) -> Option<TypeNode<'_>> {
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
pub struct TypeDeclNode<'a>(pub &'a Tree);

impl TypeDeclNode<'_> {
    pub fn name(&self) -> Option<UpperId> {
        self.find("name")?.at(0)?.upper_id()
    }

    pub fn args(&self) -> Option<Vec<LowerId>> {
        self.find("args")?.traverse(Node::lower_id)
    }

    pub fn decl_type(&self) -> Option<TypeDeclEnum<'_>> {
        let place = self.find("type")?.at(0)?;
        match place.kind()? {
            TypeSynonym => place.to().map(TypeDeclEnum::Synonym),
            TypeSum => place.to().map(TypeDeclEnum::Sum),
            TypeProduct => place.to().map(TypeDeclEnum::Product),
            _ => None,
        }
    }
}
*/
