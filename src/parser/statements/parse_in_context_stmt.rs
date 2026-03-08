use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_in_context_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_nv(TokenType::Keyword, "in")?;
        if self.peek().lexeme == "context" {
            self.advance();
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "context".into(),
                found: self.peek().clone(),
                idx: self.idx,
            });
        }
        let target = self.parse_expr_with_struct_literal_guard(false)?;
        self.expect(TokenType::LeftBrace)?;

        let mut arms = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let value = self.parse_expression(0)?;
            self.expect(TokenType::ShortArrow)?;
            let body = self.parse_block_or_expr_body()?;
            arms.push(ContextArm { value, body });
        }
        self.expect(TokenType::RightBrace)?;

        Ok(Stmt::InContext { target, arms })
    }

    // note: parse on sequence block

}
