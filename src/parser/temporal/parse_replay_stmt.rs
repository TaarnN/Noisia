use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_replay_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("replay")?;
        let recording = self.parse_expr_with_struct_literal_guard(false)?;
        let body = self.parse_block()?;
        Ok(Stmt::Replay { recording, body })
    }


}
