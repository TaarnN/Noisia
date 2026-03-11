use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_temporal_handle_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("handle")?;
        self.expect_word("temporal")?;
        self.expect_word("effects")?;
        self.expect_word("in")?;

        let body = self.parse_block()?;

        self.expect_word("with")?;
        self.expect(TokenType::LeftBrace)?;
        let mut handlers = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
                continue;
            }
            let clause = self.parse_temporal_clause()?;
            handlers.push(clause);
        }
        self.expect(TokenType::RightBrace)?;

        Ok(Stmt::TemporalHandle { body, handlers })
    }

    // note: parse if/elif/else statement
}
