use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_snapshot_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("snapshot")?;

        let mut name = None;
        let mut metadata = None;

        if self.peek().token_type == TokenType::StringLiteral
            || self.peek().token_type == TokenType::MultilineStringLiteral
        {
            name = Some(self.parse_string_literal_value()?);
        }

        if self.match_tnv(TokenType::Keyword, "with")
            || self.peek().token_type == TokenType::LeftBrace
        {
            let meta = if self.peek().token_type == TokenType::LeftBrace {
                self.parse_named_record_literal("metadata")?
            } else {
                self.parse_expression(0)?
            };
            metadata = Some(meta);
        }

        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }

        Ok(Stmt::Snapshot { name, metadata })
    }

    // note: parse rollback statement
}
