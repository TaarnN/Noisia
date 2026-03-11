use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_replay_modify_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("modify")?;
        let target = self.parse_expression(1)?;
        self.expect_nv(TokenType::Operator, "=")?;
        let value = self.parse_expression(0)?;
        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }
        Ok(Stmt::ReplayModify { target, value })
    }
}
