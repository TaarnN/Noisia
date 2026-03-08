use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_temporal_scope_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("temporal")?;
        self.expect_word("scope")?;

        let name = if self.peek().token_type == TokenType::StringLiteral
            || self.peek().token_type == TokenType::MultilineStringLiteral
        {
            Some(self.parse_string_literal_value()?)
        } else {
            None
        };

        let (config, body) = self.parse_temporal_config_and_body()?;
        Ok(Stmt::TemporalScope { name, config, body })
    }

    // note: parse temporal transaction statement

}
