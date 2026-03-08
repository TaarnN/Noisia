use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_temporal_clause(&mut self) -> ParseResult<TemporalClause> {
        self.expect_word("on")?;
        let pattern = self.parse_temporal_pattern()?;
        let guard = if self.match_tnv(TokenType::Keyword, "if") {
            Some(self.parse_expression(0)?)
        } else {
            None
        };
        self.expect(TokenType::ShortArrow)?;
        let body = self.parse_block_or_expr_body()?;
        Ok(TemporalClause::Parsed {
            pattern,
            guard,
            body,
        })
    }

    // note: parse match temporal state { ... } into raw patterns + bodies

}
