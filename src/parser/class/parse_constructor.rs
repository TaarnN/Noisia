use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_constructor(
        &mut self,
        attributes: Vec<String>,
        visibility: Visibility,
    ) -> ParseResult<ConstructorDecl> {
        self.expect_nv(TokenType::Keyword, "init")?;

        let ctor_name = if self.match_one(TokenType::Dot) {
            Some(self.expect(TokenType::Identifier)?.lexeme)
        } else {
            None
        };

        self.expect(TokenType::LeftParen)?;
        let params = self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightParen,
            |this| {
                let pattern = this.parse_pattern()?;
                let mut typ = None;
                if this.match_one(TokenType::Colon) {
                    typ = Some(this.parse_type()?);
                }
                let mut default = None;
                if this.match_tnv(TokenType::Operator, "=") {
                    default = Some(this.parse_expression(0)?);
                }
                Ok(Param {
                    pattern,
                    typ,
                    default,
                })
            },
        )?;
        self.expect(TokenType::RightParen)?;

        let throws = self.match_word("throws");

        if self.peek().token_type != TokenType::LeftBrace {
            return Err(self.error_here("Constructors must have a block body"));
        }
        let body = self.parse_block()?;

        Ok(ConstructorDecl {
            attributes,
            visibility,
            name: ctor_name,
            params,
            throws,
            body,
        })
    }

    // note: read ModulePath or ident::ident

}
