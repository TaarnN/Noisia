use super::super::*;

impl Parser {
    pub(in crate::parser) fn is_range_end_delimiter(&self) -> bool {
        if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "by" {
            return true;
        }

        matches!(
            self.peek().token_type,
            TokenType::RightBracket
                | TokenType::RightParen
                | TokenType::RightBrace
                | TokenType::Comma
                | TokenType::Semicolon
                | TokenType::ShortArrow
                | TokenType::FatArrow
                | TokenType::EOF
        )
    }

    // note: unary then primary+postfix

}
