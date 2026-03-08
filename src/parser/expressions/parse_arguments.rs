use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_arguments(&mut self) -> ParseResult<Vec<Expr>> {
        self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightParen,
            |this| this.parse_expression(0),
        )
    }

    // note: parse generic args until >

}
