use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_gc_temporal_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("gc")?;
        if !self.match_word("temporal") {
            let expr = self.parse_postfix(Expr::Ident("gc".to_string()))?;
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
            }
            return Ok(Stmt::Expr(expr));
        }
        let filter = if self.match_word("where") {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Stmt::GcTemporal { filter })
    }


}
