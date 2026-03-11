use super::super::*;

impl Parser {
    pub(in crate::parser) fn find_top_level_short_arrow_before_arm_terminator(
        &self,
        start_idx: usize,
    ) -> Option<usize> {
        let mut depth_paren: i32 = 0;
        let mut depth_brace: i32 = 0;
        let mut depth_bracket: i32 = 0;
        let mut i = start_idx;

        while i < self.tokens.len() {
            let tok = &self.tokens[i];
            let at_top = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;

            if at_top {
                if tok.token_type == TokenType::ShortArrow {
                    return Some(i);
                }

                if tok.token_type == TokenType::RightBrace
                    || tok.token_type == TokenType::Comma
                    || tok.token_type == TokenType::Semicolon
                {
                    return None;
                }
            }

            match tok.token_type {
                TokenType::LeftParen => depth_paren += 1,
                TokenType::RightParen => depth_paren = depth_paren.saturating_sub(1),
                TokenType::LeftBrace => depth_brace += 1,
                TokenType::RightBrace => depth_brace = depth_brace.saturating_sub(1),
                TokenType::LeftBracket => depth_bracket += 1,
                TokenType::RightBracket => depth_bracket = depth_bracket.saturating_sub(1),
                _ => {}
            }

            i += 1;
        }

        None
    }
}
