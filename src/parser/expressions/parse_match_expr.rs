use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_match_expr(&mut self) -> ParseResult<Expr> {
        self.expect_nv(TokenType::Keyword, "match")?;
        let expr = self.parse_expr_with_struct_literal_guard(false)?;
        self.expect(TokenType::LeftBrace)?;

        let mut arms = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            if self.peek().token_type == TokenType::Comma
                || self.peek().token_type == TokenType::Semicolon
            {
                self.advance();
                continue;
            }

            let pattern = self.parse_pattern()?;
            let guard = if self.match_tnv(TokenType::Keyword, "if") {
                Some(self.parse_expression(0)?)
            } else {
                None
            };
            self.expect(TokenType::ShortArrow)?;
            let body = self.parse_match_arm_body_expr()?;
            arms.push(MatchArm {
                pattern,
                guard,
                body,
            });

            if self.peek().token_type == TokenType::Comma
                || self.peek().token_type == TokenType::Semicolon
            {
                self.advance();
            }
        }

        self.expect(TokenType::RightBrace)?;
        Ok(Expr::MatchExpr {
            expr: Box::new(expr),
            arms,
        })
    }
}
