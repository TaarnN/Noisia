use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_batch_temporal_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("batch")?;
        self.expect_word("temporal")?;

        let body = self.parse_block()?;

        let mut optimize = None;
        if self.match_tnv(TokenType::Keyword, "optimize") {
            let _ = self.match_tnv(TokenType::Keyword, "for");
            let opt = if self.peek().token_type == TokenType::LeftBrace {
                self.parse_named_record_literal("optimize")?
            } else {
                self.parse_expression(0)?
            };
            optimize = Some(opt);
        }

        Ok(Stmt::BatchTemporal { body, optimize })
    }

    // note: parse temporal scope statement

}
