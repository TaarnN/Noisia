use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_try_catch_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("try")?;
        let try_block = self.parse_block()?;
        self.expect_word("catch")?;

        let catch_name = if self.match_one(TokenType::LeftParen) {
            let name = self.expect_ident_like()?.lexeme;
            self.expect(TokenType::RightParen)?;
            name
        } else {
            self.expect_ident_like()?.lexeme
        };

        let catch_block = self.parse_block()?;

        Ok(Stmt::TryCatch {
            try_block,
            catch_name,
            catch_block,
        })
    }

    // note: parse string literal value
}
