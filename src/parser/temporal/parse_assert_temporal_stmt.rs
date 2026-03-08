use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_assert_temporal_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("assert")?;
        if !self.match_word("temporal") {
            let expr = self.parse_postfix(Expr::Ident("assert".to_string()))?;
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
            }
            return Ok(Stmt::Expr(expr));
        }
        let kind_tok = self.expect_ident_like()?;
        let body = self.parse_block()?;
        Ok(Stmt::AssertTemporal {
            kind: kind_tok.lexeme,
            body,
        })
    }


}
