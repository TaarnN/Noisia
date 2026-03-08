use super::super::*;

impl Parser {
    pub(in crate::parser) fn block_starts_as_record_literal(&self, start_idx: usize) -> bool {
        if !matches!(
            self.tokens.get(start_idx),
            Some(t) if t.token_type == TokenType::LeftBrace
        ) {
            return false;
        }

        match (self.tokens.get(start_idx + 1), self.tokens.get(start_idx + 2)) {
            (Some(t1), _) if t1.token_type == TokenType::RightBrace => true,
            (
                Some(t1),
                Some(t2),
            ) if matches!(t1.token_type, TokenType::Identifier | TokenType::Keyword)
                && (t2.token_type == TokenType::Colon
                    || t2.token_type == TokenType::Comma
                    || t2.token_type == TokenType::RightBrace) =>
            {
                true
            }
            _ => false,
        }
    }

    // note: check if a block is followed by another block

}
