use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_expression(&mut self, min_prec: u8) -> ParseResult<Expr> {
        let left = self.parse_unary_or_primary()?;
        self.parse_expression_with_left(left, min_prec)
    }
}
