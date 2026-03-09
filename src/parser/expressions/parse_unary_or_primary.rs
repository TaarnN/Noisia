use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_unary_or_primary(&mut self) -> ParseResult<Expr> {
        let tok = self.peek().clone();

        if tok.token_type == TokenType::Operator && (tok.lexeme == ".." || tok.lexeme == "..=") {
            let inclusive = tok.lexeme == "..=";
            self.advance();
            let end = if self.is_range_end_delimiter() {
                None
            } else {
                Some(self.parse_expression(0)?)
            };
            let step = if self.match_tnv(TokenType::Keyword, "by") {
                Some(Box::new(self.parse_expression(0)?))
            } else {
                None
            };

            return Ok(Expr::Range {
                start: None,
                end: end.map(Box::new),
                inclusive,
                step,
            });
        }

        if matches!(tok.token_type, TokenType::Keyword | TokenType::Identifier)
            && tok.lexeme == "await"
        {
            self.advance();
            let rhs = self.parse_expression(13)?;
            return Ok(Expr::Await(Box::new(rhs)));
        }

        if matches!(tok.token_type, TokenType::Keyword | TokenType::Identifier)
            && tok.lexeme == "spawn"
        {
            self.advance();
            let rhs = self.parse_expression(13)?;
            return Ok(Expr::Spawn(Box::new(rhs)));
        }

        if matches!(tok.token_type, TokenType::Keyword | TokenType::Identifier)
            && tok.lexeme == "try"
        {
            self.advance();
            let rhs = self.parse_expression(13)?;
            return Ok(Expr::Try(Box::new(rhs)));
        }

        if tok.token_type == TokenType::Operator && tok.lexeme == "@new" {
            self.advance();
            return self.parse_pointer_new_with(PointerType::ManagedPointer);
        }

        if tok.token_type == TokenType::Operator {
            if tok.lexeme == "~"
                && matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.token_type == TokenType::Operator && t.lexeme == "&"
                )
            {
                self.advance();
                self.advance();
                let rhs = self.parse_expression(100)?;
                return Ok(Expr::PointerRef {
                    pointer_type: PointerType::RawPointer,
                    expr: Box::new(rhs),
                });
            }

            if matches!(tok.lexeme.as_str(), "~" | "+" | "&")
                && matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.token_type == TokenType::Identifier && t.lexeme == "new"
                )
                && matches!(
                    self.tokens.get(self.idx + 2),
                    Some(t) if t.token_type == TokenType::LeftParen
                )
            {
                let pointer_type = match tok.lexeme.as_str() {
                    "~" => PointerType::RawPointer,
                    "+" => PointerType::SharedPointer,
                    "&" => PointerType::WeakPointer,
                    _ => PointerType::ManagedPointer,
                };
                self.advance();
                self.advance();
                return self.parse_pointer_new_with(pointer_type);
            }

            if tok.lexeme == "*" {
                self.advance();
                if matches!(
                    self.peek().token_type,
                    TokenType::RightParen
                        | TokenType::Comma
                        | TokenType::RightBracket
                        | TokenType::RightBrace
                        | TokenType::Semicolon
                        | TokenType::EOF
                ) {
                    return Ok(Expr::Ident("*".to_string()));
                }
                let rhs = self.parse_expression(100)?;
                return Ok(Expr::PointerDeref {
                    expr: Box::new(rhs),
                    safe: false,
                });
            }
        }

        if matches!(tok.token_type, TokenType::Operator) {
            if tok.lexeme == "-" || tok.lexeme == "+" || tok.lexeme == "!" || tok.lexeme == "not" {
                let op = self.advance().lexeme.clone();
                let rhs = self.parse_expression(100)?;
                return Ok(Expr::Unary {
                    op,
                    rhs: Box::new(rhs),
                });
            }
        }

        let primary = self.parse_primary()?;
        self.parse_postfix(primary)
    }


}
