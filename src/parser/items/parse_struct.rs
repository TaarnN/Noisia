use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_struct(&mut self, attributes: Vec<String>) -> ParseResult<StructDecl> {
        self.expect_nv(TokenType::Keyword, "struct")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        self.expect(TokenType::LeftBrace)?;
        let fields = self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightBrace,
            |this| {
                let field_name = this.expect(TokenType::Identifier)?.lexeme;
                this.expect(TokenType::Colon)?;
                let typ = this.parse_type()?;
                Ok((field_name, typ))
            },
        )?;
        self.expect(TokenType::RightBrace)?;
        Ok(StructDecl {
            attributes,
            name,
            generics,
            fields,
        })
    }

    // note: parse enum with variants

}
