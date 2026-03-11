use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_tokens_until_short_arrow(
        &mut self,
        context: &str,
    ) -> ParseResult<Vec<Token>> {
        let mut raw = Vec::new();
        let mut paren_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut bracket_depth = 0usize;

        while !self.is_at_end() {
            let at_top = paren_depth == 0 && brace_depth == 0 && bracket_depth == 0;
            if at_top && self.peek().token_type == TokenType::ShortArrow {
                break;
            }

            if at_top && self.peek().token_type == TokenType::RightBrace {
                return Err(self.error_here(format!("Expected ':>' in {}", context)));
            }

            let tok = self.advance().clone();
            match tok.token_type {
                TokenType::LeftParen => paren_depth += 1,
                TokenType::RightParen => paren_depth = paren_depth.saturating_sub(1),
                TokenType::LeftBrace => brace_depth += 1,
                TokenType::RightBrace => brace_depth = brace_depth.saturating_sub(1),
                TokenType::LeftBracket => bracket_depth += 1,
                TokenType::RightBracket => bracket_depth = bracket_depth.saturating_sub(1),
                _ => {}
            }
            raw.push(tok);
        }

        if raw.is_empty() {
            return Err(self.error_here(format!("Expected {} before ':>'", context)));
        }

        Ok(raw)
    }
}
