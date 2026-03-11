use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_auto_checkpoint_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("auto")?;
        if !self.match_word("checkpoint") {
            let expr = self.parse_postfix(Expr::Ident("auto".to_string()))?;
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
            }
            return Ok(Stmt::Expr(expr));
        }
        self.expect_word("before")?;
        let body = self.parse_block()?;
        Ok(Stmt::AutoCheckpoint { body })
    }
}
