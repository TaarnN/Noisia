use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_pattern_list(
        &mut self,
        end: TokenType,
    ) -> ParseResult<(Vec<Pattern>, Vec<String>)> {
        let mut patterns = Vec::new();
        let mut bindings = Vec::new();
        while !self.is_at_end() && self.peek().token_type != end {
            let sub_pat = self.parse_pattern()?;
            bindings.extend(sub_pat.bindings.clone());
            patterns.push(sub_pat);
            if self.match_one(TokenType::Comma) {
                if self.peek().token_type == end {
                    break;
                }
                continue;
            } else {
                break;
            }
        }
        self.expect(end)?;
        Ok((patterns, bindings))
    }

    // note: parse struct pattern fields
}
