use super::super::*;

impl Parser {
    pub(in crate::parser) fn expect_word(&mut self, v: &str) -> ParseResult<Token> {
        let tok = self.peek().clone();
        if (tok.token_type == TokenType::Keyword || tok.token_type == TokenType::Identifier)
            && tok.lexeme == v
        {
            Ok(self.advance().clone())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("'{}'", v),
                found: tok,
                idx: self.idx,
            })
        }
    }
}
