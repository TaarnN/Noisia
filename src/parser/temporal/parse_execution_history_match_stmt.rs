use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_execution_history_match_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("match")?;
        self.expect_word("execution")?;
        self.expect_word("history")?;
        self.expect(TokenType::LeftBrace)?;

        let mut clauses = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            if self.peek().token_type == TokenType::Semicolon
                || self.peek().token_type == TokenType::Comma
            {
                self.advance();
                continue;
            }

            let raw_pattern = self.parse_tokens_until_short_arrow("execution history pattern")?;
            self.expect(TokenType::ShortArrow)?;
            let body = self.parse_block_or_expr_body()?;

            clauses.push(TemporalClause::Parsed {
                pattern: TemporalPattern::Raw(raw_pattern),
                guard: None,
                body,
            });
        }

        self.expect(TokenType::RightBrace)?;
        Ok(Stmt::TemporalMatch {
            target: Expr::Literal(Literal::String("execution history".to_string())),
            clauses,
        })
    }

    // note: parse handle temporal effects statement

}
