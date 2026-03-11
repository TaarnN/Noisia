use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_cleanup_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("cleanup")?;
        if self.peek().token_type == TokenType::LeftParen {
            let expr = self.parse_postfix(Expr::Ident("cleanup".to_string()))?;
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
            }
            return Ok(Stmt::Expr(expr));
        }
        if self.peek().token_type != TokenType::LeftBrace {
            let expr = self.parse_expression(0)?;
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
            }
            return Ok(Stmt::Expr(expr));
        }
        let body = self.parse_block()?;
        Ok(Stmt::Cleanup { body })
    }

    // note: parse batch temporal statement
}
