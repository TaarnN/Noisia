use super::super::*;

impl Parser {
    pub(in crate::parser) fn expect_gt(&mut self) -> ParseResult<Token> {
        if self.match_gt() {
            Ok(self.tokens[self.idx.saturating_sub(1)].clone())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "Operator(>)".into(),
                found: self.peek().clone(),
                idx: self.idx,
            })
        }
    }

    // note: read all @attrs into list
}
