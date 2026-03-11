use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_for_pattern(&mut self) -> ParseResult<Pattern> {
        let first = self.parse_pattern()?;
        if !self.match_one(TokenType::Comma) {
            return Ok(first);
        }

        let mut patterns = vec![first];
        let mut bindings = patterns[0].bindings.clone();

        loop {
            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "in" {
                return Err(self.error_here("Expected pattern before 'in'"));
            }
            let pat = self.parse_pattern()?;
            bindings.extend(pat.bindings.clone());
            patterns.push(pat);
            if self.match_one(TokenType::Comma) {
                continue;
            }
            break;
        }

        Ok(Pattern {
            kind: PatternKind::Tuple(patterns),
            bindings,
        })
    }

    // note: parse any statement
}
