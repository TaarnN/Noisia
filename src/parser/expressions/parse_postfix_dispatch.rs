use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_postfix_dispatch(&mut self, left: Expr) -> ParseResult<Expr> {
        let mut expr = left;
        loop {
            match self.peek().token_type {
                TokenType::FatArrow if self.peek().lexeme == "->" => {
                    self.advance();
                    expr = self.parse_pointer_deref_postfix(expr, false)?;
                }
                TokenType::Operator if self.peek().lexeme == "->?" => {
                    self.advance();
                    expr = self.parse_pointer_deref_postfix(expr, true)?;
                }
                TokenType::Operator if self.peek().lexeme == "?." => {
                    self.advance();
                    if self.peek().token_type == TokenType::LeftParen {
                        self.advance();
                        let args = self.parse_arguments()?;
                        self.expect(TokenType::RightParen)?;
                        expr = Expr::OptionalCall {
                            callee: Box::new(expr),
                            generics: Vec::new(),
                            args,
                        };
                        continue;
                    }

                    let field_or_method = self.expect_ident_like()?.lexeme;
                    let mut generics = Vec::new();
                    if self.peek().token_type == TokenType::Operator && self.peek().lexeme == "<" {
                        self.advance();
                        generics = self.parse_generic_args()?;
                    }

                    if self.peek().token_type == TokenType::LeftParen {
                        self.advance();
                        let args = self.parse_arguments()?;
                        self.expect(TokenType::RightParen)?;
                        let callee = Expr::FieldAccess {
                            object: Box::new(expr),
                            field: field_or_method.clone(),
                        };
                        expr = Expr::OptionalCall {
                            callee: Box::new(callee),
                            generics,
                            args,
                        };
                    } else {
                        if !generics.is_empty() {
                            return Err(self.error_here(
                                "Unexpected generics without optional call",
                            ));
                        }
                        expr = Expr::OptionalFieldAccess {
                            object: Box::new(expr),
                            field: field_or_method,
                        };
                    }
                }
                TokenType::Dot => {
                    self.advance();
                    let field_or_method = self.expect_ident_like()?.lexeme;
                    let mut generics = Vec::new();
                    if self.peek().token_type == TokenType::Operator && self.peek().lexeme == "<" {
                        self.advance();
                        generics = self.parse_generic_args()?;
                    }
                    if self.peek().token_type == TokenType::LeftParen {
                        self.advance();
                        let args = self.parse_arguments()?;
                        self.expect(TokenType::RightParen)?;
                        expr = Expr::MethodCall {
                            object: Box::new(expr),
                            method: field_or_method,
                            generics,
                            args,
                        };
                    } else {
                        if !generics.is_empty() {
                            return Err(self.error_here(
                                "Unexpected generics without method call",
                            ));
                        }
                        expr = Expr::FieldAccess {
                            object: Box::new(expr),
                            field: field_or_method,
                        };
                    }
                }
                TokenType::LeftBracket => {
                    self.advance();
                    if let Some(op_idx) = self.find_slice_operator(self.idx) {
                        let start = if op_idx == self.idx {
                            None
                        } else {
                            Some(Box::new(self.parse_expression_slice(op_idx)?))
                        };

                        self.advance();

                        let end = if self.peek().token_type == TokenType::RightBracket {
                            None
                        } else {
                            Some(Box::new(self.parse_expression(0)?))
                        };

                        self.expect(TokenType::RightBracket)?;
                        expr = Expr::Slice {
                            target: Box::new(expr),
                            start,
                            end,
                        };
                    } else {
                        let index_expr = self.parse_expression(0)?;
                        self.expect(TokenType::RightBracket)?;
                        expr = Expr::Index {
                            array: Box::new(expr),
                            index: Box::new(index_expr),
                        };
                    }
                }
                TokenType::Operator if self.peek().lexeme == "!" => {
                    if let Expr::Ident(name) = &expr {
                        if matches!(
                            self.tokens.get(self.idx + 1),
                            Some(t) if t.token_type == TokenType::LeftParen
                        ) {
                            self.advance();
                            self.expect(TokenType::LeftParen)?;
                            let args = self.parse_arguments()?;
                            self.expect(TokenType::RightParen)?;
                            expr = Expr::MacroCall {
                                name: name.clone(),
                                args,
                            };
                            continue;
                        }
                    }
                    break;
                }
                TokenType::Operator if self.peek().lexeme == "<" => {
                    // note: look ahead to confirm <...>( ) call
                    let mut depth: i32 = 0;
                    let mut found_generic_call = false;
                    for la in 0..64usize {
                        if let Some(t) = self.tokens.get(self.idx + la) {
                            if t.token_type == TokenType::Operator && t.lexeme == "<" {
                                depth += 1;
                            } else if t.token_type == TokenType::Operator && t.lexeme == ">" {
                                depth -= 1;
                                if depth == 0 {
                                    if matches!(
                                        self.tokens.get(self.idx + la + 1),
                                        Some(n) if n.token_type == TokenType::LeftParen
                                    ) {
                                        found_generic_call = true;
                                    }
                                    break;
                                }
                            } else if t.token_type == TokenType::Operator && t.lexeme == ">>" {
                                depth -= 2;
                                if depth <= 0 {
                                    if matches!(
                                        self.tokens.get(self.idx + la + 1),
                                        Some(n) if n.token_type == TokenType::LeftParen
                                    ) {
                                        found_generic_call = true;
                                    }
                                    break;
                                }
                            }
                        } else {
                            break;
                        }
                    }

                    if !found_generic_call {
                        break;
                    }

                    self.advance();
                    let generics = self.parse_generic_args()?;
                    self.expect(TokenType::LeftParen)?;
                    let args = self.parse_arguments()?;
                    self.expect(TokenType::RightParen)?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        generics,
                        args,
                    };
                }
                TokenType::LeftParen => {
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(TokenType::RightParen)?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        generics: Vec::new(),
                        args,
                    };
                }
                TokenType::LeftBrace => {
                    if !self.allow_trailing_closures {
                        break;
                    }

                    let call_like = matches!(
                        &expr,
                        Expr::Call { .. } | Expr::MethodCall { .. } | Expr::OptionalCall { .. }
                    );
                    let field_style = matches!(
                        &expr,
                        Expr::FieldAccess { .. } | Expr::OptionalFieldAccess { .. }
                    );

                    if !call_like && !field_style {
                        break;
                    }

                    if field_style && !self.looks_like_trailing_closure_with_params() {
                        break;
                    }

                    let closure = self.parse_trailing_closure_lambda()?;
                    expr = Self::attach_trailing_closure_arg(expr, closure);
                }
                TokenType::Keyword if self.peek().lexeme == "with" => {
                    if !matches!(
                        self.tokens.get(self.idx + 1),
                        Some(t) if t.token_type == TokenType::LeftBrace
                    ) {
                        break;
                    }

                    // note: record update: base with { fields }
                    self.advance();
                    self.expect(TokenType::LeftBrace)?;
                    let fields = self.parse_struct_fields()?;

                    expr = Expr::Literal(Literal::Struct {
                        name: "update".to_string(),
                        base: Some(Box::new(expr)),
                        fields,
                    });
                }
                TokenType::Keyword if self.peek().lexeme == "at" => {
                    if let Expr::PointerNew {
                        pointer_type,
                        expr: inner,
                        at,
                        expires,
                    } = expr
                    {
                        self.advance();
                        let value = self.parse_expression(0)?;
                        expr = Expr::PointerNew {
                            pointer_type,
                            expr: inner,
                            at: Some(Box::new(value)),
                            expires,
                        };
                    } else {
                        break;
                    }
                }
                TokenType::Keyword if self.peek().lexeme == "expires" => {
                    if let Expr::PointerNew {
                        pointer_type,
                        expr: inner,
                        at,
                        expires,
                    } = expr
                    {
                        self.advance();
                        let value = self.parse_expression(0)?;
                        expr = Expr::PointerNew {
                            pointer_type,
                            expr: inner,
                            at,
                            expires: Some(Box::new(value)),
                        };
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    // note: parse call argument list

}
