use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_debug_temporal_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("debug")?;
        self.expect_word("temporal")?;
        self.expect(TokenType::LeftBrace)?;

        let mut clauses = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let raw = self.parse_debug_temporal_clause_tokens()?;
            clauses.push(TemporalClause::Raw(raw));
        }

        self.expect(TokenType::RightBrace)?;
        Ok(Stmt::DebugTemporal { clauses })
    }

    // note: parse temporal pattern
}
