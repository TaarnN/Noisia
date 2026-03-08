use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_expression_slice(&mut self, end_idx: usize) -> ParseResult<Expr> {
        let sub_tokens = self.tokens[self.idx..end_idx].to_vec();
        let mut sub_parser = Parser::new(sub_tokens);
        let expr = sub_parser.parse_expression(0)?;

        if !sub_parser.is_at_end() {
            let tok = sub_parser.peek().clone();
            return Err(ParseError::UnexpectedToken {
                expected: "end of list comprehension expression".into(),
                found: tok,
                idx: self.idx + sub_parser.idx,
            });
        }

        self.idx = end_idx;
        Ok(expr)
    }

    // note: parse calls, fields, index, update

}
