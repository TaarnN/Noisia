use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_commit_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("commit")?;
        if self.peek().token_type == TokenType::LeftParen {
            let expr = self.parse_postfix(Expr::Ident("commit".to_string()))?;
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
            }
            return Ok(Stmt::Expr(expr));
        }
        let mut metadata = None;
        if self.match_word("with") {
            let meta = if self.peek().token_type == TokenType::LeftBrace {
                self.parse_named_record_literal("metadata")?
            } else {
                self.parse_expression(0)?
            };
            metadata = Some(meta);
        }
        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }
        Ok(Stmt::Commit { metadata })
    }
}
