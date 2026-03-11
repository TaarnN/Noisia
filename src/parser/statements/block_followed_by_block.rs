use super::super::*;

impl Parser {
    pub(in crate::parser) fn block_followed_by_block(&self, start_idx: usize) -> bool {
        if !matches!(
            self.tokens.get(start_idx),
            Some(t) if t.token_type == TokenType::LeftBrace
        ) {
            return false;
        }

        let mut depth = 0usize;
        let mut i = start_idx;
        while i < self.tokens.len() {
            match self.tokens[i].token_type {
                TokenType::LeftBrace => depth += 1,
                TokenType::RightBrace => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return matches!(
                            self.tokens.get(i + 1),
                            Some(t) if t.token_type == TokenType::LeftBrace
                        );
                    }
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    // note: parse optional config block then required body block
}
