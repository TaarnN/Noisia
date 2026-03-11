use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_replay_pause_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("pause")?;
        let mut each = false;
        let mut checkpoint = None;
        if self.match_word("at") {
            if self.match_word("each") {
                self.expect_word("checkpoint")?;
                each = true;
            } else {
                checkpoint = Some(self.parse_expression(0)?);
            }
        }
        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }
        Ok(Stmt::ReplayPause { each, checkpoint })
    }
}
