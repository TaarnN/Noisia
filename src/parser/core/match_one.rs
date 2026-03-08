use super::super::*;

impl Parser {
    pub(in crate::parser) fn match_one(&mut self, t: TokenType) -> bool {
        if self.peek().token_type == t {
            self.advance();
            true
        } else {
            false
        }
    }

    // note: eat token by type and text

}
