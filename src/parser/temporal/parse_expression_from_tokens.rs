use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_expression_from_tokens(
        &self,
        tokens: Vec<Token>,
        min_prec: u8,
        context: &str,
    ) -> ParseResult<Expr> {
        if tokens.is_empty() {
            return Err(self.error_here(format!("Expected {}", context)));
        }
        let mut sub_parser = Parser::new(tokens);
        let expr = sub_parser.parse_expression(min_prec)?;
        if !sub_parser.is_at_end() {
            return Err(ParseError::UnexpectedToken {
                expected: format!("end of {}", context),
                found: sub_parser.peek().clone(),
                idx: sub_parser.idx,
            });
        }
        Ok(expr)
    }
}
