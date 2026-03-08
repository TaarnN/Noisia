use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_if_expr_tail(&mut self) -> ParseResult<Expr> {
        let cond = self.parse_expr_with_struct_literal_guard(false)?;
        let then_block = self.parse_block_or_expr_body()?;
        let then_expr = Expr::Block(then_block);

        let else_branch = if self.match_tnv(TokenType::Keyword, "elif") {
            Some(Box::new(self.parse_if_expr_tail()?))
        } else if self.match_tnv(TokenType::Keyword, "else") {
            let else_block = self.parse_block_or_expr_body()?;
            Some(Box::new(Expr::Block(else_block)))
        } else {
            None
        };

        Ok(Expr::IfExpr {
            cond: Box::new(cond),
            then_branch: Box::new(then_expr),
            else_branch,
        })
    }


}
