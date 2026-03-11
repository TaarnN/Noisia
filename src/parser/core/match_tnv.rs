use super::super::*;

impl Parser {
    pub(in crate::parser) fn match_tnv(&mut self, t: TokenType, ch: &str) -> bool {
        let token = self.peek();
        if (token.token_type == t) && (token.lexeme == ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    // note: handle > or split >> for generics
}
