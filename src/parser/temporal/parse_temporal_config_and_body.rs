use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_temporal_config_and_body(
        &mut self,
    ) -> ParseResult<(Option<Expr>, Block)> {
        if self.peek().token_type != TokenType::LeftBrace {
            return Err(self.error_here("Expected temporal block"));
        }

        let config = if self.block_followed_by_block(self.idx) {
            Some(self.parse_named_record_literal("config")?)
        } else {
            None
        };

        let body = self.parse_block()?;
        Ok((config, body))
    }

    // note: shared rewind parser for statement/expression forms
}
