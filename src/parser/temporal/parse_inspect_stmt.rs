use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_inspect_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("inspect")?;

        let mut target = None;
        let mut filter = None;
        let mut body = None;

        let should_parse_target = !matches!(
            self.peek().token_type,
            TokenType::LeftBrace | TokenType::Semicolon | TokenType::RightBrace | TokenType::EOF
        );

        if should_parse_target {
            let start_idx = self.idx;
            let mut i = self.idx;
            while i < self.tokens.len() {
                let tok = &self.tokens[i];
                if tok.token_type == TokenType::LeftBrace
                    || tok.token_type == TokenType::Semicolon
                    || tok.token_type == TokenType::RightBrace
                    || (tok.token_type == TokenType::Keyword && tok.lexeme == "where")
                {
                    break;
                }
                i += 1;
            }

            if i == start_idx {
                return Err(self.error_here("Expected inspect target"));
            }

            if i - start_idx == 1 {
                let sub_tokens = self.tokens[start_idx..i].to_vec();
                let mut sub_parser = Parser::new(sub_tokens);
                let expr = sub_parser.parse_expression(0)?;
                if !sub_parser.is_at_end() {
                    return Err(self.error_here("Invalid inspect target"));
                }
                self.idx = i;
                target = Some(expr);
            } else {
                let mut phrase = String::new();
                for (idx, tok) in self.tokens[start_idx..i].iter().enumerate() {
                    if idx > 0 {
                        phrase.push(' ');
                    }
                    phrase.push_str(&tok.lexeme);
                }
                self.idx = i;
                target = Some(Expr::Literal(Literal::String(phrase)));
            }
        }

        if self.match_tnv(TokenType::Keyword, "where") {
            filter = Some(self.parse_block()?);
        }

        if self.peek().token_type == TokenType::LeftBrace {
            body = Some(self.parse_block()?);
        }

        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }

        Ok(Stmt::Inspect {
            target,
            filter,
            body,
        })
    }

    // note: parse snapshot statement

}
