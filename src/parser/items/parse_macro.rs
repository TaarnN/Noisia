use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_macro(
        &mut self,
        attributes: Vec<String>,
    ) -> ParseResult<MacroDecl> {
        self.expect_nv(TokenType::Keyword, "macro")?;
        let name = self.expect_ident_like()?;

        // Parse !(params) part - this should be name! not just !
        if !(self.peek().token_type == TokenType::Operator && self.peek().lexeme == "!") {
            return Err(self.error_here("Expected '!' after macro name".to_string()));
        }
        self.advance(); // consume '!'

        // Parse (params) part
        let mut params = Vec::new();
        if self.match_one(TokenType::LeftParen) {
            params = self.parse_comma_separated(
                |tok| tok.token_type == TokenType::RightParen,
                |this| Ok(this.expect(TokenType::Identifier)?.lexeme),
            )?;
            self.expect(TokenType::RightParen)?;
        }

        let body = self.parse_block()?;
        Ok(MacroDecl {
            attributes,
            name: name.lexeme,
            params,
            body,
        })
    }

    // note: parse extension (name: Type) { ... } or extension Type { ... }
}
