use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_temporal_transaction_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_word("temporal")?;
        self.expect_word("transaction")?;

        let (config, body) = self.parse_temporal_config_and_body()?;

        let mut catch_name = None;
        let mut catch_block = None;
        if self.match_word("catch") {
            if self.peek().token_type == TokenType::LeftBrace {
                catch_block = Some(self.parse_block()?);
            } else {
                let name = if self.match_one(TokenType::LeftParen) {
                    let n = self.expect_ident_like()?.lexeme;
                    self.expect(TokenType::RightParen)?;
                    n
                } else {
                    self.expect_ident_like()?.lexeme
                };
                catch_name = Some(name);
                catch_block = Some(self.parse_block()?);
            }
        }

        Ok(Stmt::TemporalTransaction {
            config,
            body,
            catch_name,
            catch_block,
        })
    }

    // note: parse temporal test statement
}
