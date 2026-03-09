use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_stmt_dispatch(&mut self) -> ParseResult<Stmt> {
        if matches!(
            self.peek().token_type,
            TokenType::Attribute | TokenType::ParameterizedAttribute
        ) {
            let attributes = self.parse_attributes();
            let stmt = self.parse_statement()?;
            return Ok(Stmt::Attributed {
                attributes,
                stmt: Box::new(stmt),
            });
        }

        if self.peek().lexeme == "on"
            && matches!(
                self.tokens.get(self.idx + 1),
                Some(t) if t.lexeme == "checkpoint"
            )
        {
            return self.parse_replay_on_checkpoint_stmt();
        }

        if self.peek().lexeme == "in"
            && matches!(
                self.tokens.get(self.idx + 1),
                Some(t) if t.lexeme == "context"
            )
        {
            return self.parse_in_context_stmt();
        }

        if self.peek().lexeme == "on"
            && matches!(
                self.tokens.get(self.idx + 1),
                Some(t) if t.token_type == TokenType::Keyword && t.lexeme == "sequence"
            )
        {
            return self.parse_on_sequence_stmt();
        }

        match self.peek().lexeme.as_str() {
            "compile-time" => {
                self.expect_nv(TokenType::Keyword, "compile-time")?;
                let body = self.parse_block()?;
                Ok(Stmt::CompileTimeBlock { body })
            }
            "let" => self.parse_let_stmt(),
            "using" => {
                self.expect_word("using")?;
                let target = self.parse_expression(0)?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Using { target })
            }
            "try" => self.parse_try_catch_stmt(),
            "continue" => {
                self.expect_nv(TokenType::Keyword, "continue")?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Continue)
            }
            "break" => {
                self.expect_nv(TokenType::Keyword, "break")?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Break)
            }
            "panic" => {
                self.expect_nv(TokenType::Keyword, "panic")?;
                self.expect_nv(TokenType::Keyword, "unless")?;
                let condition = self.parse_expression(0)?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::PanicUnless { condition })
            }
            "return" => {
                self.advance();
                let expr = if self.peek().token_type != TokenType::Semicolon {
                    Some(self.parse_expression(0)?)
                } else {
                    None
                };
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Return(expr))
            }
            "while" => {
                self.advance();
                let cond = self.parse_expr_with_struct_literal_guard(false)?;
                let body = self.parse_block()?;
                Ok(Stmt::While { cond, body })
            }
            "checkpoint" => {
                if self.is_checkpoint_stmt_start() {
                    self.parse_checkpoint_stmt()
                } else {
                    let expr = self.parse_expression(0)?;
                    if self.peek().token_type == TokenType::Semicolon {
                        self.advance();
                    }
                    Ok(Stmt::Expr(expr))
                }
            }
            "rewind" => self.parse_rewind_stmt(),
            "retry" => self.parse_retry_stmt(),
            "inspect" => self.parse_inspect_stmt(),
            "snapshot" => self.parse_snapshot_stmt(),
            "rollback" => self.parse_rollback_stmt(),
            "replay" => self.parse_replay_stmt(),
            "pause" => self.parse_replay_pause_stmt(),
            "modify" => self.parse_replay_modify_stmt(),
            "merge" => self.parse_merge_branch_stmt(),
            "auto" => self.parse_auto_checkpoint_stmt(),
            "emit" => self.parse_emit_stmt(),
            "gc" => self.parse_gc_temporal_stmt(),
            "assert" => self.parse_assert_temporal_stmt(),
            "commit" => self.parse_commit_stmt(),
            "cleanup" => self.parse_cleanup_stmt(),
            "batch" => {
                if matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.lexeme == "temporal"
                ) {
                    self.parse_batch_temporal_stmt()
                } else {
                    let expr = self.parse_expression(0)?;
                    if self.peek().token_type == TokenType::Semicolon {
                        self.advance();
                    }
                    Ok(Stmt::Expr(expr))
                }
            }
            "temporal" => {
                if matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.lexeme == "scope"
                ) {
                    self.parse_temporal_scope_stmt()
                } else if matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.lexeme == "transaction"
                ) {
                    self.parse_temporal_transaction_stmt()
                } else if matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.lexeme == "test"
                ) {
                    self.parse_temporal_test_stmt()
                } else if matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.lexeme == "memory"
                ) {
                    self.parse_temporal_memory_stmt()
                } else {
                    let expr = self.parse_expression(0)?;
                    if self.peek().token_type == TokenType::Semicolon {
                        self.advance();
                    }
                    Ok(Stmt::Expr(expr))
                }
            }
            "for" => {
                self.advance();
                let pattern = self.parse_for_pattern()?;
                self.expect_nv(TokenType::Keyword, "in")?;

                let iter = self.parse_expr_with_struct_literal_guard(false)?;

                let body = if self.match_one(TokenType::ShortArrow) {
                    self.parse_block_or_expr_body()?
                } else {
                    self.parse_block()?
                };
                Ok(Stmt::For {
                    pattern,
                    iter,
                    body,
                })
            }
            "loop" => {
                self.advance();
                let body = self.parse_block()?;
                Ok(Stmt::Loop { body })
            }
            "match" => {
                if matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t)
                        if (t.token_type == TokenType::Keyword
                            || t.token_type == TokenType::Identifier)
                            && t.lexeme == "temporal"
                ) && matches!(
                    self.tokens.get(self.idx + 2),
                    Some(t)
                        if (t.token_type == TokenType::Keyword
                            || t.token_type == TokenType::Identifier)
                            && t.lexeme == "state"
                ) {
                    self.parse_temporal_match_stmt()
                } else if matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t)
                        if (t.token_type == TokenType::Keyword
                            || t.token_type == TokenType::Identifier)
                            && t.lexeme == "execution"
                ) && matches!(
                    self.tokens.get(self.idx + 2),
                    Some(t)
                        if (t.token_type == TokenType::Keyword
                            || t.token_type == TokenType::Identifier)
                            && t.lexeme == "history"
                ) {
                    self.parse_execution_history_match_stmt()
                } else {
                    self.expect_nv(TokenType::Keyword, "match")?;

                    if self.peek().token_type == TokenType::LeftBrace {
                        self.advance();
                        let mut arms = Vec::new();
                        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
                            if self.peek().token_type == TokenType::Semicolon {
                                self.advance();
                                continue;
                            }
                            let condition = self.parse_match_condition_expr()?;
                            self.expect(TokenType::ShortArrow)?;
                            let body = self.parse_block_or_expr_body()?;
                            arms.push(MatchCondArm { condition, body });
                            if self.peek().token_type == TokenType::Comma
                                || self.peek().token_type == TokenType::Semicolon
                            {
                                self.advance();
                            }
                        }
                        self.expect(TokenType::RightBrace)?;
                        Ok(Stmt::MatchCond { arms })
                    } else {
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
                        Ok(Stmt::Match { expr, arms })
                    }
                }
            }
            "if" => self.parse_if_stmt(),
            "scope" => {
                self.expect_nv(TokenType::Keyword, "scope")?;
                let body = self.parse_block()?;
                Ok(Stmt::Scope { body })
            }
            "defer" => {
                self.expect_nv(TokenType::Keyword, "defer")?;
                let expr = self.parse_expression(0)?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Defer { expr })
            }
            "debug" => {
                if matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.lexeme == "temporal"
                ) {
                    self.parse_debug_temporal_stmt()
                } else {
                    let expr = self.parse_expression(0)?;
                    if self.peek().token_type == TokenType::Semicolon {
                        self.advance();
                    }
                    Ok(Stmt::Expr(expr))
                }
            }
            "handle" => {
                if matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.lexeme == "temporal"
                ) && matches!(
                    self.tokens.get(self.idx + 2),
                    Some(t) if t.lexeme == "effects"
                ) {
                    self.parse_temporal_handle_stmt()
                } else {
                    let expr = self.parse_expression(0)?;
                    if self.peek().token_type == TokenType::Semicolon {
                        self.advance();
                    }
                    Ok(Stmt::Expr(expr))
                }
            }
            "watch" => {
                self.expect_nv(TokenType::Keyword, "watch")?;

                let mut variables = Vec::new();
                loop {
                    variables.push(self.parse_expr_with_struct_literal_guard(false)?);
                    if self.match_one(TokenType::Comma) {
                        continue;
                    } else {
                        break;
                    }
                }

                self.expect(TokenType::LeftBrace)?;

                let mut clauses = Vec::new();
                while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
                    self.expect_nv(TokenType::Keyword, "when")?;
                    let condition = self.parse_expression(0)?;
                    self.expect(TokenType::ShortArrow)?;
                    let body = self.parse_block_or_expr_body()?;

                    clauses.push(WatchClause { condition, body });
                }

                self.expect(TokenType::RightBrace)?;
                Ok(Stmt::Watch { variables, clauses })
            }
            "converge" => {
                self.expect_nv(TokenType::Keyword, "converge")?;
                self.expect_nv(TokenType::Keyword, "with")?;

                let history = self.match_tnv(TokenType::Keyword, "history");
                let variable = self.expect(TokenType::Identifier)?.lexeme;

                let body = self.parse_block()?;
                self.expect_nv(TokenType::Keyword, "until")?;
                let until = self.parse_expression(0)?;

                Ok(Stmt::Converge {
                    history,
                    variable,
                    body,
                    until,
                })
            }
            "within" => {
                self.expect_nv(TokenType::Keyword, "within")?;
                let time = self.parse_expression(2)?;

                let condition = if self.match_tnv(TokenType::Keyword, "if") {
                    Some(self.parse_expression(0)?)
                } else {
                    None
                };

                if self.match_tnv(TokenType::Operator, "or") || self.match_word("or") {
                    let fallback = if self.is_word("rewind") {
                        self.parse_rewind_expr_relaxed()?
                    } else {
                        self.parse_expression(0)?
                    };
                    let body = self.parse_block()?;
                    Ok(Stmt::Within {
                        time,
                        condition,
                        body,
                        fallback: Some(fallback),
                    })
                } else if self.peek().token_type == TokenType::LeftBrace {
                    let body = self.parse_block()?;
                    Ok(Stmt::Within {
                        time,
                        condition,
                        body,
                        fallback: None,
                    })
                } else {
                    self.expect(TokenType::ShortArrow)?;
                    let body = self.parse_block_or_expr_body()?;
                    Ok(Stmt::Within {
                        time,
                        condition,
                        body,
                        fallback: None,
                    })
                }
            }
            "atomically" => {
                self.expect(TokenType::Keyword)?;
                let body = self.parse_block()?;
                Ok(Stmt::Atomically { body })
            }
            "trap" => {
                self.expect_nv(TokenType::Keyword, "trap")?;
                let error_condition = self.parse_expression(0)?;
                self.expect(TokenType::ShortArrow)?;
                let body = self.parse_block_or_expr_body()?;
                Ok(Stmt::Trap {
                    error_condition,
                    body,
                })
            }
            "guard" => {
                self.expect_nv(TokenType::Keyword, "guard")?;
                let condition = self.parse_expression(0)?;
                let mut then_block = Block { stmts: Vec::new() };
                if self.match_one(TokenType::ShortArrow) {
                    then_block = self.parse_block_or_expr_body()?;
                }

                let else_block = if self.match_word("else") {
                    if self.match_one(TokenType::ShortArrow) {
                        Some(self.parse_block_or_expr_body()?)
                    } else if self.is_word("rewind") {
                        let expr = self.parse_rewind_expr_relaxed()?;
                        Some(Block {
                            stmts: vec![Stmt::Expr(expr)],
                        })
                    } else {
                        let expr = self.parse_expression(0)?;
                        Some(Block {
                            stmts: vec![Stmt::Expr(expr)],
                        })
                    }
                } else {
                    None
                };

                if then_block.stmts.is_empty() && else_block.is_none() {
                    return Err(self.error_here("Expected ':>' or 'else' in guard"));
                }

                Ok(Stmt::Guard {
                    condition,
                    then_block,
                    else_block,
                })
            }
            "poll" => {
                self.expect_nv(TokenType::Keyword, "poll")?;
                let signal = self.parse_expression(0)?;
                self.expect_nv(TokenType::Keyword, "with")?;
                if self.peek().lexeme == "interval" {
                    self.advance();
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "interval".into(),
                        found: self.peek().clone(),
                        idx: self.idx,
                    });
                }
                self.expect_nv(TokenType::Operator, "=")?;
                let interval = self.parse_expression(0)?;
                self.expect(TokenType::ShortArrow)?;
                let body = self.parse_block_or_expr_body()?;

                Ok(Stmt::Poll {
                    signal,
                    interval,
                    body,
                })
            }
            "delete" => {
                self.expect(TokenType::Keyword)?;
                let expr = self.parse_expression(0)?;

                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }

                Ok(Stmt::Delete { expr })
            }
            "joins" => {
                self.expect(TokenType::Keyword)?;
                self.expect(TokenType::LeftParen)?;
                let mut decls = Vec::new();
                while self.peek().token_type != TokenType::RightParen && !self.is_at_end() {
                    let decl = self.parse_let_stmt()?;
                    decls.push(decl);
                    if !self.match_one(TokenType::Semicolon) {
                        continue;
                    }
                }
                self.expect(TokenType::RightParen)?;
                let mut body = self.parse_block()?;

                // note: joins auto-delete only the bindings declared in joins(...)
                let mut delete_names = Vec::new();
                for decl in &decls {
                    if let Stmt::Let { pattern, .. } = decl {
                        delete_names.extend(pattern.bindings.clone());
                    }
                }
                for name in delete_names {
                    body.stmts.push(Stmt::Delete {
                        expr: Expr::Ident(name),
                    });
                }

                Ok(Stmt::Joins { decls, body })
            }
            _ => {
                let expr = self.parse_expression(0)?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Expr(expr))
            }
        }
    }

    // note: parse in context block
}
