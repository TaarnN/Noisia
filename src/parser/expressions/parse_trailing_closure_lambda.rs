use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_trailing_closure_lambda(&mut self) -> ParseResult<Expr> {
        self.expect(TokenType::LeftBrace)?;

        let mut params = Vec::new();
        if self.peek().token_type != TokenType::RightBrace {
            let checkpoint = self.idx;
            let mut names = Vec::new();
            let mut has_param_list = false;

            if matches!(
                self.peek().token_type,
                TokenType::Identifier | TokenType::Keyword
            ) && self.peek().lexeme != "in"
            {
                has_param_list = true;
                names.push(self.advance().lexeme.clone());
                while self.match_one(TokenType::Comma) {
                    names.push(self.expect_ident_like()?.lexeme);
                }
            }

            if has_param_list && self.match_word("in") {
                params = names
                    .into_iter()
                    .map(Self::lambda_param_from_name)
                    .collect();
            } else {
                self.idx = checkpoint;
            }
        }

        let mut stmts = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            if self.peek().token_type == TokenType::Comma {
                self.advance();
                continue;
            }

            let stmt = self.parse_statement()?;
            stmts.push(stmt);

            if self.peek().token_type == TokenType::Comma {
                self.advance();
            }
        }

        self.expect(TokenType::RightBrace)?;

        let body = if stmts.len() == 1 {
            match stmts.pop().unwrap() {
                Stmt::Expr(expr) => expr,
                stmt => Expr::Block(Block { stmts: vec![stmt] }),
            }
        } else {
            Expr::Block(Block { stmts })
        };

        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
        })
    }


}
