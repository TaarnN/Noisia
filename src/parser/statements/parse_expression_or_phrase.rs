use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_expression_or_phrase(&mut self, stop_words: &[&str]) -> ParseResult<Expr> {
        if self.should_parse_phrase_on_line(stop_words) {
            self.parse_phrase_expr_on_line(stop_words)
        } else {
            self.parse_expression(0)
        }
    }

    // note: parse comma list until end

}
