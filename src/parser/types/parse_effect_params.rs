use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_effect_params(&mut self) -> ParseResult<Vec<TypeRef>> {
        self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightParen,
            |this| this.parse_type(),
        )
    }

    // note: parse where constraints

}
