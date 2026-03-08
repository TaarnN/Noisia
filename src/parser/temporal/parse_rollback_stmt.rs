use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_rollback_stmt(&mut self) -> ParseResult<Stmt> {
        let rollback_tok = self.expect_word("rollback")?;
        let rollback_line = rollback_tok.line;

        let mut subject = None;
        let mut target = None;
        let mut condition = None;
        let mut metadata = None;
        let mut query = None;

        if self.match_tnv(TokenType::Keyword, "to") {
            target = Some(self.parse_expression_or_phrase(&["if", "with", "where"])?);
        } else if !matches!(
            self.peek().token_type,
            TokenType::Semicolon | TokenType::RightBrace | TokenType::EOF
        ) && !(self.peek().token_type == TokenType::Keyword
            && (self.peek().lexeme == "if" || self.peek().lexeme == "with"))
            && self.peek().line == rollback_line
        {
            subject = Some(self.parse_expression_or_phrase(&["to", "if", "with", "where"])?);
            if self.match_word("to") {
                target = Some(self.parse_expression_or_phrase(&["if", "with", "where"])?);
            }
        }

        loop {
            if self.match_word("if") {
                condition = Some(self.parse_expression(0)?);
                continue;
            }

            if self.match_word("with") {
                let meta = if self.peek().token_type == TokenType::LeftBrace {
                    self.parse_named_record_literal("metadata")?
                } else {
                    self.parse_expression(0)?
                };
                metadata = Some(meta);
                continue;
            }

            if self.match_word("where") {
                query = Some(self.parse_block()?);
                continue;
            }

            break;
        }

        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }

        Ok(Stmt::Rollback {
            subject,
            target,
            condition,
            metadata,
            query,
        })
    }

    // note: parse replay statement

}
