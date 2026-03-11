use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_lambda(&mut self) -> ParseResult<Expr> {
        let mut params = Vec::new();

        if self.match_one(TokenType::Dot) {
            let implicit_name = "it".to_string();
            params.push(Param {
                pattern: Pattern {
                    kind: PatternKind::Identifier(implicit_name.clone()),
                    bindings: vec![implicit_name.clone()],
                },
                typ: None,
                default: None,
            });

            let field = self.expect_ident_like()?.lexeme;
            let base = Expr::Ident(implicit_name);
            let expr = Expr::FieldAccess {
                object: Box::new(base),
                field,
            };
            let expr = self.parse_postfix(expr)?;
            let body = self.parse_expression_with_left(expr, 0)?;

            return Ok(Expr::Lambda {
                params,
                body: Box::new(body),
            });
        }

        // note: support no-arg lambda forms: \:> ... and \_ :> ...
        if self.peek().token_type != TokenType::ShortArrow
            && !(self.peek().token_type == TokenType::Identifier
                && self.peek().lexeme == "_"
                && matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.token_type == TokenType::ShortArrow
                ))
        {
            let param_name = self.expect(TokenType::Identifier)?.lexeme;
            params.push(Param {
                pattern: Pattern {
                    kind: PatternKind::Identifier(param_name.clone()),
                    bindings: vec![param_name],
                },
                typ: None,
                default: None,
            });

            while self.match_one(TokenType::Comma) {
                let param_name = self.expect(TokenType::Identifier)?.lexeme;
                params.push(Param {
                    pattern: Pattern {
                        kind: PatternKind::Identifier(param_name.clone()),
                        bindings: vec![param_name],
                    },
                    typ: None,
                    default: None,
                });
            }
        } else if self.peek().token_type == TokenType::Identifier && self.peek().lexeme == "_" {
            self.advance();
        }

        self.expect(TokenType::ShortArrow)?;

        let body = self.parse_expression(0)?;

        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
        })
    }

    // note: parse macro name!(params) { ... }
}
