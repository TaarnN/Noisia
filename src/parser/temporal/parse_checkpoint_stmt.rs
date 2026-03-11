use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_checkpoint_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("checkpoint")?;

        let mut name = None;
        let mut metadata = None;
        let mut body = None;
        let mut preserve = None;

        if self.peek().token_type == TokenType::StringLiteral
            || self.peek().token_type == TokenType::MultilineStringLiteral
        {
            name = Some(self.parse_string_literal_value()?);
        }

        if self.match_word("with") {
            let meta = if self.is_word("config")
                && matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.token_type == TokenType::LeftBrace
                ) {
                self.advance();
                self.parse_named_record_literal("config")?
            } else if self.peek().token_type == TokenType::LeftBrace {
                self.parse_named_record_literal("metadata")?
            } else {
                self.parse_expression(0)?
            };
            metadata = Some(meta);
        }

        if self.peek().token_type == TokenType::LeftBrace {
            body = Some(self.parse_block()?);
        }

        if self.match_word("as") {
            let label = self.parse_string_literal_value()?;
            if name.is_some() {
                return Err(self.error_here("Checkpoint name already specified"));
            }
            name = Some(label);
        }

        if self.match_word("preserve") {
            preserve = if self.peek().token_type == TokenType::LeftBrace {
                if self.block_starts_as_record_literal(self.idx) {
                    let payload = self.parse_named_record_literal("preserve")?;
                    Some(Block {
                        stmts: vec![Stmt::Expr(payload)],
                    })
                } else {
                    Some(self.parse_block()?)
                }
            } else {
                Some(self.parse_block()?)
            };
        }

        // Allow trailing execution body after preserve payload.
        if body.is_none() && self.peek().token_type == TokenType::LeftBrace {
            body = Some(self.parse_block()?);
        }

        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }

        Ok(Stmt::Checkpoint {
            name,
            metadata,
            body: body.unwrap_or(Block { stmts: Vec::new() }),
            preserve,
        })
    }

    // note: parse rewind statement
}
