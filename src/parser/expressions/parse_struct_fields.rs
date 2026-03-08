use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_struct_fields(&mut self) -> ParseResult<Vec<(String, Expr)>> {
        let fields = self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightBrace,
            |this| {
                let field_name = this.expect_ident_like()?.lexeme;
                let field_expr = if this.match_one(TokenType::Colon) {
                    this.parse_expression(0)?
                } else {
                    if this.peek().token_type != TokenType::Comma
                        && this.peek().token_type != TokenType::RightBrace
                    {
                        return Err(this.error_here("Expected ':' or ',' in struct literal"));
                    }
                    Expr::Ident(field_name.clone())
                };
                Ok((field_name, field_expr))
            },
        )?;
        self.expect(TokenType::RightBrace)?;
        Ok(fields)
    }

    // note: parse type with generics and nullable

}
