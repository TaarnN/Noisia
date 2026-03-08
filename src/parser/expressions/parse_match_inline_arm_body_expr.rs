use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_match_inline_arm_body_expr(&mut self) -> ParseResult<Expr> {
        let mut raw = Vec::new();
        let mut paren_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut bracket_depth = 0usize;

        while !self.is_at_end() {
            let tok = self.peek().clone();
            let at_top = paren_depth == 0 && brace_depth == 0 && bracket_depth == 0;

            if at_top {
                if tok.token_type == TokenType::RightBrace
                    || tok.token_type == TokenType::Comma
                    || tok.token_type == TokenType::Semicolon
                {
                    break;
                }

                if !raw.is_empty() && self.looks_like_match_arm_start(self.idx) {
                    break;
                }
            }

            let consumed = self.advance().clone();
            match consumed.token_type {
                TokenType::LeftParen => paren_depth += 1,
                TokenType::RightParen => paren_depth = paren_depth.saturating_sub(1),
                TokenType::LeftBrace => brace_depth += 1,
                TokenType::RightBrace => brace_depth = brace_depth.saturating_sub(1),
                TokenType::LeftBracket => bracket_depth += 1,
                TokenType::RightBracket => bracket_depth = bracket_depth.saturating_sub(1),
                _ => {}
            }
            raw.push(consumed);
        }

        self.parse_expression_from_tokens(raw, 0, "match arm body")
    }


}
