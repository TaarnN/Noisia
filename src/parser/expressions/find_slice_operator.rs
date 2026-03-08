use super::super::*;

impl Parser {
    pub(in crate::parser) fn find_slice_operator(&self, start_idx: usize) -> Option<usize> {
        let mut depth_paren: i32 = 0;
        let mut depth_brace: i32 = 0;
        let mut depth_bracket: i32 = 0;
        let mut i = start_idx;

        while i < self.tokens.len() {
            let tok = &self.tokens[i];
            let at_top = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;

            if at_top {
                match tok.token_type {
                    TokenType::RightBracket => return None,
                    TokenType::Operator if tok.lexeme == ".." || tok.lexeme == "..=" => {
                        return Some(i)
                    }
                    _ => {}
                }
            }

            match tok.token_type {
                TokenType::LeftParen => depth_paren += 1,
                TokenType::RightParen => {
                    if depth_paren > 0 {
                        depth_paren -= 1;
                    }
                }
                TokenType::LeftBrace => depth_brace += 1,
                TokenType::RightBrace => {
                    if depth_brace > 0 {
                        depth_brace -= 1;
                    }
                }
                TokenType::LeftBracket => depth_bracket += 1,
                TokenType::RightBracket => {
                    if depth_bracket > 0 {
                        depth_bracket -= 1;
                    }
                }
                _ => {}
            }

            i += 1;
        }

        None
    }

    // note: parse expression from current idx up to end_idx (exclusive)

}
