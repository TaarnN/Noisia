use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_merge_branch_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("merge")?;
        if !self.match_word("branch") {
            let expr = self.parse_postfix(Expr::Ident("merge".to_string()))?;
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
            }
            return Ok(Stmt::Expr(expr));
        }
        let branch = self.parse_expression(0)?;
        self.expect_word("to")?;
        let target = self.parse_expression(0)?;
        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }
        Ok(Stmt::MergeBranch { branch, target })
    }


}
