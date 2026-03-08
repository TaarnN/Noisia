use super::super::*;

impl Parser {
    pub(in crate::parser) fn expect(&mut self, expected: TokenType) -> ParseResult<Token> {
        let tok = self.peek().clone();
        if tok.token_type == expected {
            Ok(self.advance().clone())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", expected),
                found: tok,
                idx: self.idx,
            })
        }
    }

    // note: create a generic parse error at current token

}
