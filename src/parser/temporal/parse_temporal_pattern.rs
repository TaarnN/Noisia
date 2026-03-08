use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_temporal_pattern(&mut self) -> ParseResult<TemporalPattern> {
        let kind_tok = self.expect_ident_like()?;
        let mut args = Vec::new();
        if self.match_one(TokenType::LeftParen) {
            if self.peek().token_type != TokenType::RightParen {
                args = self.parse_arguments()?;
            }
            self.expect(TokenType::RightParen)?;
        }

        let condition = if self.match_tnv(TokenType::Keyword, "with")
            || self.match_tnv(TokenType::Keyword, "where")
        {
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        Ok(TemporalPattern::Parsed {
            kind: kind_tok.lexeme,
            args,
            condition,
        })
    }

    // note: parse temporal handler clause

}
