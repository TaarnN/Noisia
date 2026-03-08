use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_block(&mut self) -> ParseResult<Block> {
        self.expect(TokenType::LeftBrace)?;
        let mut stmts = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            if self.peek().token_type == TokenType::Comma {
                self.advance();
                continue;
            }
            let s = self.parse_statement()?;
            stmts.push(s);
            if self.peek().token_type == TokenType::Comma {
                self.advance();
            }
        }
        self.expect(TokenType::RightBrace)?;
        Ok(Block { stmts })
    }

    // note: parse block or short expr

}
