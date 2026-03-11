use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_property_accessors(
        &mut self,
    ) -> ParseResult<(Option<Block>, Option<String>, Option<Block>)> {
        self.expect(TokenType::LeftBrace)?;
        let mut getter = None;
        let mut setter_param = None;
        let mut setter_body = None;

        while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
            if self.is_word("get") {
                self.advance();
                let body = if self.match_one(TokenType::ShortArrow) {
                    let expr = self.parse_expression(0)?;
                    Block {
                        stmts: vec![Stmt::Return(Some(expr))],
                    }
                } else if self.peek().token_type == TokenType::LeftBrace {
                    self.parse_block()?
                } else {
                    return Err(self.error_here("Expected getter body"));
                };
                getter = Some(body);
            } else if self.is_word("set") {
                self.advance();
                self.expect(TokenType::LeftParen)?;
                let param_name = self.expect_ident_like()?.lexeme;
                if self.match_one(TokenType::Colon) {
                    let _ = self.parse_type()?;
                }
                self.expect(TokenType::RightParen)?;
                let body = if self.match_one(TokenType::ShortArrow) {
                    let expr = self.parse_expression(0)?;
                    Block {
                        stmts: vec![Stmt::Expr(expr)],
                    }
                } else if self.peek().token_type == TokenType::LeftBrace {
                    self.parse_block()?
                } else {
                    return Err(self.error_here("Expected setter body"));
                };
                setter_param = Some(param_name);
                setter_body = Some(body);
            } else {
                return Err(
                    self.error_here(format!("Unsupported property accessor: {}", self.peek()))
                );
            }

            if self.peek().token_type == TokenType::Comma
                || self.peek().token_type == TokenType::Semicolon
            {
                self.advance();
            }
        }

        self.expect(TokenType::RightBrace)?;
        Ok((getter, setter_param, setter_body))
    }

    // note: parse init with params and block
}
