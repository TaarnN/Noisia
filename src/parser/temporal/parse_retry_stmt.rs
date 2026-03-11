use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_retry_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("retry")?;
        self.expect_word("with")?;
        self.expect_word("rewind")?;
        self.expect_word("up")?;
        self.expect_word("to")?;
        let max_times = self.parse_expression(0)?;
        self.expect_word("times")?;
        let body = self.parse_block()?;
        Ok(Stmt::TemporalRetry { max_times, body })
    }
}
