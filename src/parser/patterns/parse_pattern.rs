use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        self.parse_pattern_dispatch()
    }
}
