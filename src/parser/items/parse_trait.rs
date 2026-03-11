use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_trait(
        &mut self,
        attributes: Vec<String>,
    ) -> ParseResult<TraitDecl> {
        self.expect_nv(TokenType::Keyword, "trait")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        self.expect(TokenType::LeftBrace)?;
        let mut methods = Vec::new();

        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let method_attributes = self.parse_attributes();
            let method_visibility = self.parse_visibility();
            let func = self.parse_function_with(method_attributes, method_visibility)?;
            methods.push(func);
        }

        self.expect(TokenType::RightBrace)?;

        Ok(TraitDecl {
            attributes,
            name,
            methods,
        })
    }

    // note: parse interface with generics and methods
}
