use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_temporal_test_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("temporal")?;
        self.expect_word("test")?;

        let name = if self.peek().token_type == TokenType::StringLiteral
            || self.peek().token_type == TokenType::MultilineStringLiteral
        {
            self.parse_string_literal_value()?
        } else {
            self.expect_ident_like()?.lexeme
        };

        let (config, body) = self.parse_temporal_config_and_body()?;
        Ok(Stmt::TemporalTest { name, config, body })
    }

    // note: parse temporal memory statement
}
