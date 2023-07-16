use super::{resolved::ResolvedQualified, *};

pub trait Walkable {
    fn walk(&mut self, visitor: &mut dyn Visitor);
}

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
    fn visit_arm(&mut self, node: &mut Arm);
    fn visit_ident(&mut self, node: &mut Ident);
    fn visit_qualified(&mut self, node: &mut Qualified);
    fn visit_block(&mut self, node: &mut Block);
    fn visit_variant(&mut self, node: &mut Variant);
    fn visit_let_case(&mut self, node: &mut LetCase);
    fn visit_enum_decl(&mut self, node: &mut EnumDecl);
    fn visit_field(&mut self, node: &mut Field);
    fn visit_record_decl(&mut self, node: &mut RecordDecl);
    fn visit_use_decl(&mut self, node: &mut UseDecl);
    fn visit_type_def(&mut self, node: &mut TypeDef);
    fn visit_use_decl_kind(&mut self, node: &mut UseDecl);
    fn visit_binder(&mut self, node: &mut Binder);
    fn visit_program(&mut self, node: &mut Program);
    fn visit_resolved_qualified(&mut self, node: &mut ResolvedQualified);
    fn visit_pattern_impl(&mut self, node: &mut Box<dyn PatternImpl>);
    fn visit_literal_impl(&mut self, node: &mut Box<dyn LiteralImpl>);
    fn visit_statement_impl(&mut self, node: &mut Box<dyn StatementImpl>);
    fn visit_expr_impl(&mut self, node: &mut Box<dyn ExprImpl>);
    fn visit_type_impl(&mut self, node: &mut Box<dyn TypeImpl>);
}

impl<T> Visitor for T {
    default fn visit_upper_type(&mut self, node: &mut UpperType) {
        node.walk(self);
    }
    default fn visit_lower_type(&mut self, node: &mut LowerType) {
        node.walk(self);
    }
    default fn visit_type_arrow(&mut self, node: &mut TypeArrow) {
        node.walk(self);
    }
    default fn visit_type_application(&mut self, node: &mut TypeApplication) {
        node.walk(self);
    }
    default fn visit_type_forall(&mut self, node: &mut TypeForall) {
        node.walk(self);
    }
    default fn visit_str_literal(&mut self, node: &mut StrLiteral) {
        node.walk(self);
    }
    default fn visit_char_literal(&mut self, node: &mut CharLiteral) {
        node.walk(self);
    }
    default fn visit_int_literal(&mut self, node: &mut IntLiteral) {
        node.walk(self);
    }
    default fn visit_float_literal(&mut self, node: &mut FloatLiteral) {
        node.walk(self);
    }
    default fn visit_unit_literal(&mut self, node: &mut UnitLiteral) {
        node.walk(self);
    }
    default fn visit_pat_annotation(&mut self, node: &mut PatAnnotation) {
        node.walk(self);
    }
    default fn visit_pat_or(&mut self, node: &mut PatOr) {
        node.walk(self);
    }
    default fn visit_pat_application(&mut self, node: &mut PatApplication) {
        node.walk(self);
    }
    default fn visit_pat_wildcard(&mut self, node: &mut PatWildcard) {
        node.walk(self);
    }
    default fn visit_pat_upper(&mut self, node: &mut PatUpper) {
        node.walk(self);
    }
    default fn visit_pat_lower(&mut self, node: &mut PatLower) {
        node.walk(self);
    }
    default fn visit_pat_literal(&mut self, node: &mut PatLiteral) {
        node.walk(self);
    }
    default fn visit_stmt_expr(&mut self, node: &mut StmtExpr) {
        node.walk(self);
    }
    default fn visit_stmt_let(&mut self, node: &mut StmtLet) {
        node.walk(self);
    }
    default fn visit_expr_literal(&mut self, node: &mut ExprLiteral) {
        node.walk(self);
    }
    default fn visit_lambda_expr(&mut self, node: &mut LambdaExpr) {
        node.walk(self);
    }
    default fn visit_application_expr(&mut self, node: &mut ApplicationExpr) {
        node.walk(self);
    }
    default fn visit_when_expr(&mut self, node: &mut WhenExpr) {
        node.walk(self);
    }
    default fn visit_annotation_expr(&mut self, node: &mut AnnotationExpr) {
        node.walk(self);
    }
    default fn visit_let_expr(&mut self, node: &mut LetExpr) {
        node.walk(self);
    }
    default fn visit_expr_ident(&mut self, node: &mut ExprIdent) {
        node.walk(self);
    }
    default fn visit_binary_expr(&mut self, node: &mut BinaryExpr) {
        node.walk(self);
    }
    default fn visit_acessor_expr(&mut self, node: &mut AcessorExpr) {
        node.walk(self);
    }
    default fn visit_block_expr(&mut self, node: &mut BlockExpr) {
        node.walk(self);
    }
    default fn visit_let_decl(&mut self, node: &mut LetDecl) {
        node.walk(self);
    }
    default fn visit_type_decl(&mut self, node: &mut TypeDecl) {
        node.walk(self);
    }
    default fn visit_unit_type(&mut self, node: &mut UnitType) {
        node.walk(self);
    }
    default fn visit_stmt_error(&mut self, node: &mut StmtError) {
        node.walk(self);
    }
    default fn visit_arm(&mut self, node: &mut Arm) {
        node.walk(self);
    }
    default fn visit_ident(&mut self, node: &mut Ident) {
        node.walk(self);
    }
    default fn visit_qualified(&mut self, node: &mut Qualified) {
        node.walk(self);
    }
    default fn visit_block(&mut self, node: &mut Block) {
        node.walk(self);
    }
    default fn visit_variant(&mut self, node: &mut Variant) {
        node.walk(self);
    }
    default fn visit_let_case(&mut self, node: &mut LetCase) {
        node.walk(self);
    }
    default fn visit_enum_decl(&mut self, node: &mut EnumDecl) {
        node.walk(self);
    }
    default fn visit_field(&mut self, node: &mut Field) {
        node.walk(self);
    }
    default fn visit_record_decl(&mut self, node: &mut RecordDecl) {
        node.walk(self);
    }
    default fn visit_use_decl(&mut self, node: &mut UseDecl) {
        node.walk(self);
    }
    default fn visit_type_def(&mut self, node: &mut TypeDef) {
        node.walk(self);
    }
    default fn visit_use_decl_kind(&mut self, node: &mut UseDecl) {
        node.walk(self);
    }
    default fn visit_program(&mut self, node: &mut Program) {
        node.walk(self);
    }
    default fn visit_binder(&mut self, node: &mut Binder) {
        node.walk(self);
    }
    default fn visit_resolved_qualified(&mut self, node: &mut ResolvedQualified) {
        node.walk(self);
    }
    default fn visit_pattern_impl(&mut self, node: &mut Box<dyn PatternImpl>) {
        node.walk(self)
    }
    default fn visit_literal_impl(&mut self, node: &mut Box<dyn LiteralImpl>) {
        node.walk(self)
    }
    default fn visit_statement_impl(&mut self, node: &mut Box<dyn StatementImpl>) {
        node.walk(self)
    }
    default fn visit_expr_impl(&mut self, node: &mut Box<dyn ExprImpl>) {
        node.walk(self)
    }
    default fn visit_type_impl(&mut self, node: &mut Box<dyn TypeImpl>) {
        node.walk(self)
    }
}
