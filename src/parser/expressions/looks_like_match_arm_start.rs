use super::super::*;

impl Parser {
    pub(in crate::parser) fn looks_like_match_arm_start(&self, start_idx: usize) -> bool {
        let Some(arrow_idx) = self.find_top_level_short_arrow_before_arm_terminator(start_idx) else {
            return false;
        };
        if arrow_idx <= start_idx {
            return false;
        }

        let pat_tokens = self.tokens[start_idx..arrow_idx].to_vec();
        if pat_tokens.is_empty() {
            return false;
        }

        let mut sub_parser = Parser::new(pat_tokens);
        sub_parser.parse_pattern().is_ok() && sub_parser.is_at_end()
    }


}
