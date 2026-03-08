use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_block_or_expr_body(&mut self) -> ParseResult<Block> {
        if self.peek().token_type == TokenType::LeftBrace {
            return self.parse_block();
        }

        let expr = self.parse_expression(0)?;
        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }

        Ok(Block {
            stmts: vec![Stmt::Expr(expr)],
        })
    }

    // note: avoid struct literal before block

}
