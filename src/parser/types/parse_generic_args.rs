use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_generic_args(&mut self) -> ParseResult<Vec<TypeRef>> {
        if self.peek().token_type == TokenType::Operator && self.peek().lexeme == ">" {
            self.expect_gt()?;
            return Ok(Vec::new());
        }
        let generics = self.parse_comma_separated(
            |tok| {
                tok.token_type == TokenType::Operator && (tok.lexeme == ">" || tok.lexeme == ">>")
            },
            |this| this.parse_type(),
        )?;
        self.expect_gt()?;
        Ok(generics)
    }

    // note: parse {field: expr} list

}
