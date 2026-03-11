use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_emit_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("emit")?;
        if self.peek().token_type == TokenType::LeftParen {
            let expr = self.parse_postfix(Expr::Ident("emit".to_string()))?;
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
            }
            return Ok(Stmt::Expr(expr));
        }
        let target = self.parse_expression(0)?;
        let body = if self.peek().token_type == TokenType::LeftBrace {
            let payload = self.parse_named_record_literal("payload")?;
            Block {
                stmts: vec![Stmt::Expr(payload)],
            }
        } else {
            self.parse_block()?
        };
        Ok(Stmt::Emit { target, body })
    }
}
