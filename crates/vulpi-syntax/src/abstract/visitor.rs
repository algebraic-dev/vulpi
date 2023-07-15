use super::*;

// Visitor and AST Implementation

pub trait Visitor {
    fn visit_upper_type(&mut self, node: &mut UpperType);
    fn visit_lower_type(&mut self, node: &mut LowerType);
    fn visit_type_arrow(&mut self, node: &mut TypeArrow);
    fn visit_type_application(&mut self, node: &mut TypeApplication);
    fn visit_type_forall(&mut self, node: &mut TypeForall);
    fn visit_str_literal(&mut self, node: &mut StrLiteral);
    fn visit_char_literal(&mut self, node: &mut CharLiteral);
    fn visit_int_literal(&mut self, node: &mut IntLiteral);
    fn visit_float_literal(&mut self, node: &mut FloatLiteral);
    fn visit_unit_literal(&mut self, node: &mut UnitLiteral);
    fn visit_pat_annotation(&mut self, node: &mut PatAnnotation);
    fn visit_pat_or(&mut self, node: &mut PatOr);
    fn visit_pat_application(&mut self, node: &mut PatApplication);
    fn visit_pat_wildcard(&mut self, node: &mut PatWildcard);
    fn visit_pat_upper(&mut self, node: &mut PatUpper);
    fn visit_pat_lower(&mut self, node: &mut PatLower);
    fn visit_pat_literal(&mut self, node: &mut PatLiteral);
    fn visit_stmt_expr(&mut self, node: &mut StmtExpr);
    fn visit_stmt_let(&mut self, node: &mut StmtLet);
    fn visit_expr_literal(&mut self, node: &mut ExprLiteral);
    fn visit_lambda_expr(&mut self, node: &mut LambdaExpr);
    fn visit_application_expr(&mut self, node: &mut ApplicationExpr);
    fn visit_when_expr(&mut self, node: &mut WhenExpr);
    fn visit_annotation_expr(&mut self, node: &mut AnnotationExpr);
    fn visit_let_expr(&mut self, node: &mut LetExpr);
    fn visit_expr_ident(&mut self, node: &mut ExprIdent);
    fn visit_binary_expr(&mut self, node: &mut BinaryExpr);
    fn visit_acessor_expr(&mut self, node: &mut AcessorExpr);
    fn visit_block_expr(&mut self, node: &mut BlockExpr);
    fn visit_let_decl(&mut self, node: &mut LetDecl);
    fn visit_type_decl(&mut self, node: &mut TypeDecl);
    fn visit_unit_type(&mut self, node: &mut UnitType);
    fn visit_stmt_error(&mut self, node: &mut StmtError);
}
