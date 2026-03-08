use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_temporal_memory_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("temporal")?;
        self.expect_word("memory")?;

        let (config, body) = self.parse_temporal_config_and_body()?;

        Ok(Stmt::TemporalMemory { config, body })
    }

    // note: detect debug temporal clause starts

}
