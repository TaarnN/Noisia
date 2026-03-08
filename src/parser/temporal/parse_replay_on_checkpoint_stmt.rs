use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_replay_on_checkpoint_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("on")?;
        self.expect_word("checkpoint")?;
        let checkpoint = self.parse_expression(0)?;
        let body = self.parse_block()?;
        Ok(Stmt::ReplayOnCheckpoint { checkpoint, body })
    }


}
