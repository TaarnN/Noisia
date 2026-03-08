use super::super::*;

impl Parser {
    pub(in crate::parser) fn is_checkpoint_stmt_start(&self) -> bool {
        match self.tokens.get(self.idx + 1) {
            Some(t) if t.token_type == TokenType::LeftBrace => true,
            Some(t)
                if t.token_type == TokenType::StringLiteral
                    || t.token_type == TokenType::MultilineStringLiteral =>
            {
                true
            }
            Some(t) if t.token_type == TokenType::Keyword && t.lexeme == "with" => true,
            Some(t) if t.token_type == TokenType::Keyword && t.lexeme == "preserve" => true,
            Some(t) if t.token_type == TokenType::Keyword && t.lexeme == "as" => true,
            _ => false,
        }
    }

    // note: eat token by type if present

}
