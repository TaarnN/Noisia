use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_primary_dispatch(&mut self) -> ParseResult<Expr> {
        let tok = self.peek().clone();

        if tok.token_type == TokenType::Keyword {
            if tok.lexeme == "if" {
                return self.parse_if_expr();
            }
            if tok.lexeme == "match" {
                return self.parse_match_expr();
            }
            if tok.lexeme == "async"
                && matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.token_type == TokenType::LeftBrace
                )
            {
                self.advance();
                let body = self.parse_block()?;
                let mut checkpoint = None;
                if self.match_word("with") {
                    self.expect_word("checkpoint")?;
                    checkpoint = Some(Box::new(self.parse_expression(0)?));
                }
                return Ok(Expr::AsyncBlock { body, checkpoint });
            }
            if tok.lexeme == "branch" {
                return self.parse_branch_expr();
            }
            if tok.lexeme == "rewind" {
                return self.parse_rewind_expr();
            }
        }

        match tok.token_type {
            TokenType::Dot => {
                self.advance();
                let name = self.expect_ident_like()?.lexeme;
                Ok(Expr::EnumCase(name))
            }
            TokenType::LambdaArrow => {
                self.advance();
                self.parse_lambda()
            }
            TokenType::IntLiteral => {
                self.advance();
                Ok(Expr::Literal(Literal::Int(tok.lexeme)))
            }
            TokenType::FloatLiteral => {
                self.advance();
                Ok(Expr::Literal(Literal::Float(tok.lexeme)))
            }
            TokenType::UnitLiteral => {
                self.advance();
                let lex = tok.lexeme;
                let mut split_idx = 0;
                for (i, ch) in lex.char_indices() {
                    if !(ch.is_ascii_digit() || ch == '.') {
                        split_idx = i;
                        break;
                    }
                }
                if split_idx == 0 {
                    return Ok(Expr::Literal(Literal::Unit {
                        v: lex,
                        u: "".into(),
                    }));
                }
                let (v, u) = lex.split_at(split_idx);
                Ok(Expr::Literal(Literal::Unit {
                    v: v.to_string(),
                    u: u.to_string(),
                }))
            }
            TokenType::InterpolatedStringStart => self.parse_interpolated_string(),
            TokenType::StringLiteral | TokenType::MultilineStringLiteral => {
                self.advance();
                let value = strip_string_delimiters(&tok.lexeme);
                Ok(Expr::Literal(Literal::String(value)))
            }
            TokenType::BoolLiteral => {
                self.advance();
                let b = tok.lexeme == "true";
                Ok(Expr::Literal(Literal::Bool(b)))
            }
            TokenType::Identifier
            | TokenType::ModulePath
            | TokenType::Keyword => {
                let ident = self.advance().lexeme.clone();
                if ident == "new" && self.peek().token_type == TokenType::LeftParen {
                    return self.parse_pointer_new_with(PointerType::ManagedPointer);
                }
                if self.allow_struct_literals
                    && self.peek().token_type == TokenType::LeftBrace
                    && self.block_starts_as_record_literal(self.idx)
                {
                    self.advance();
                    let fields = self.parse_struct_fields()?;
                    Ok(Expr::Literal(Literal::Struct {
                        name: ident,
                        base: None,
                        fields,
                    }))
                } else if ident == "v" && self.peek().token_type == TokenType::LeftBracket {
                    self.advance();
                    let elements = self.parse_comma_separated(
                        |tok| tok.token_type == TokenType::RightBracket,
                        |this| this.parse_expression(0),
                    )?;
                    self.expect(TokenType::RightBracket)?;
                    Ok(Expr::Literal(Literal::Vector(elements)))
                } else {
                    Ok(Expr::Ident(ident))
                }
            }
            TokenType::LeftParen => {
                self.advance();
                let mut exprs = Vec::new();
                let mut trailing_comma = false;
                while !self.is_at_end() && self.peek().token_type != TokenType::RightParen {
                    let expr = self.parse_expression(0)?;
                    exprs.push(expr);
                    if self.match_one(TokenType::Comma) {
                        trailing_comma = true;
                        continue;
                    } else {
                        trailing_comma = false;
                        break;
                    }
                }
                self.expect(TokenType::RightParen)?;
                if exprs.len() == 1 && !trailing_comma {
                    Ok(Expr::Grouping(Box::new(exprs.remove(0))))
                } else {
                    Ok(Expr::Literal(Literal::Tuple(exprs)))
                }
            }
            TokenType::LeftBracket => {
                self.advance();
                if self.peek().token_type == TokenType::Colon {
                    self.advance();
                    self.expect(TokenType::RightBracket)?;
                    return Ok(Expr::Literal(Literal::Map(Vec::new())));
                }
                if self.peek().token_type == TokenType::RightBracket {
                    self.advance();
                    return Ok(Expr::Literal(Literal::Array(Vec::new())));
                }

                if let Some(pipe_idx) = self.find_list_comp_pipe(self.idx) {
                    if pipe_idx == self.idx {
                        return Err(self.error_here("expected expression before list comprehension"));
                    }

                    let first_expr = self.parse_expression_slice(pipe_idx)?;
                    self.expect(TokenType::Pipe)?;

                    // note: list comp: [expr | pat <- iter, guard]
                    let pattern = self.parse_pattern()?;
                    self.expect_nv(TokenType::Operator, "<-")?;
                    let iter = self.parse_expression(0)?;

                    let guard = if self.match_one(TokenType::Comma) {
                        Some(Box::new(self.parse_expression(0)?))
                    } else {
                        None
                    };

                    self.expect(TokenType::RightBracket)?;
                    Ok(Expr::ListComp {
                        expr: Box::new(first_expr),
                        pattern,
                        iter: Box::new(iter),
                        guard,
                    })
                } else {
                    let first_expr = self.parse_expression(0)?;
                    if self.match_one(TokenType::Colon) {
                        let first_val = self.parse_expression(0)?;
                        let mut entries = vec![(first_expr, first_val)];
                        while self.match_one(TokenType::Comma) {
                            if self.peek().token_type == TokenType::RightBracket {
                                break;
                            }
                            let key = self.parse_expression(0)?;
                            self.expect(TokenType::Colon)?;
                            let val = self.parse_expression(0)?;
                            entries.push((key, val));
                        }
                        self.expect(TokenType::RightBracket)?;
                        Ok(Expr::Literal(Literal::Map(entries)))
                    } else {
                        let mut elements = vec![first_expr];
                        while self.match_one(TokenType::Comma) {
                            if self.peek().token_type == TokenType::RightBracket {
                                break;
                            }
                            let elem = self.parse_expression(0)?;
                            elements.push(elem);
                        }
                        self.expect(TokenType::RightBracket)?;
                        Ok(Expr::Literal(Literal::Array(elements)))
                    }
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "expression".into(),
                found: tok,
                idx: self.idx,
            }),
        }
    }


}
