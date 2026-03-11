use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_interpolated_string(&mut self) -> ParseResult<Expr> {
        let start_tok = self.expect(TokenType::InterpolatedStringStart)?;
        let mut parts: Vec<InterpolatedPart> = Vec::new();

        loop {
            let tok = self.peek().clone();
            match tok.token_type {
                TokenType::InterpolatedStringText => {
                    self.advance();
                    if !tok.lexeme.is_empty() {
                        parts.push(InterpolatedPart {
                            kind: InterpolatedPartKind::Text(tok.lexeme),
                        });
                    }
                }
                TokenType::InterpolatedExprStart => {
                    let expr_start = self.advance().clone();
                    let mut expr_tokens: Vec<Token> = Vec::new();

                    while !self.is_at_end()
                        && self.peek().token_type != TokenType::InterpolatedExprEnd
                    {
                        expr_tokens.push(self.advance().clone());
                    }

                    if self.peek().token_type != TokenType::InterpolatedExprEnd {
                        return Err(ParseError::Generic {
                            message: "Unterminated interpolated expression".into(),
                            line: expr_start.line,
                            column: expr_start.column,
                        });
                    }

                    self.advance(); // consume InterpolatedExprEnd

                    if expr_tokens.is_empty() {
                        return Err(ParseError::Generic {
                            message: "Empty interpolated expression".into(),
                            line: expr_start.line,
                            column: expr_start.column,
                        });
                    }

                    let mut sub_parser = Parser::new(expr_tokens);
                    let expr = sub_parser.parse_expression(0)?;

                    if !sub_parser.is_at_end() {
                        let tok = sub_parser.peek().clone();
                        return Err(ParseError::UnexpectedToken {
                            expected: "end of interpolated expression".into(),
                            found: tok,
                            idx: sub_parser.idx,
                        });
                    }

                    parts.push(InterpolatedPart {
                        kind: InterpolatedPartKind::Expr(expr),
                    });
                }
                TokenType::InterpolatedStringEnd => {
                    self.advance();
                    break;
                }
                TokenType::EOF => {
                    return Err(ParseError::Generic {
                        message: "Unterminated interpolated string".into(),
                        line: start_tok.line,
                        column: start_tok.column,
                    });
                }
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "interpolated string content".into(),
                        found: tok,
                        idx: self.idx,
                    });
                }
            }
        }

        Ok(Expr::InterpolatedString { parts })
    }

    // note: look for list comp pipe at top-level in current bracket
}
