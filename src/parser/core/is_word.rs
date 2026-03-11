use super::super::*;

impl Parser {
    pub(in crate::parser) fn is_word(&self, v: &str) -> bool {
        let tok = self.peek();
        (tok.token_type == TokenType::Keyword || tok.token_type == TokenType::Identifier)
            && tok.lexeme == v
    }

    // note: consume keyword/identifier text when present
}
