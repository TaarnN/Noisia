use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_postfix(&mut self, left: Expr) -> ParseResult<Expr> {
        self.parse_postfix_dispatch(left)
    }
}
