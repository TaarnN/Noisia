use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_primary(&mut self) -> ParseResult<Expr> {
        self.parse_primary_dispatch()
    }
}
