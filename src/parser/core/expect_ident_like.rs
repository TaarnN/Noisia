use super::super::*;

impl Parser {
    pub(in crate::parser) fn expect_ident_like(&mut self) -> ParseResult<Token> {
        let tok = self.peek().clone();
        match tok.token_type {
            TokenType::Identifier | TokenType::Keyword => Ok(self.advance().clone()),
            _ => Err(ParseError::UnexpectedToken {
                expected: "Identifier".into(),
                found: tok,
                idx: self.idx,
            }),
        }
    }

    // note: require token type with exact text

}
