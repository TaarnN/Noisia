use super::super::*;

impl Parser {
    pub(in crate::parser) fn expect_nv(&mut self, expected: TokenType, v: &str) -> ParseResult<Token> {
        let tok = self.peek().clone();
        if tok.token_type == expected && tok.lexeme == v {
            Ok(self.advance().clone())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", expected),
                found: tok,
                idx: self.idx,
            })
        }
    }

    // note: require keyword or identifier with exact text

}
