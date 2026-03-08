use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_interface(&mut self, attributes: Vec<String>) -> ParseResult<InterfaceDecl> {
        self.expect_nv(TokenType::Keyword, "interface")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        self.expect(TokenType::LeftBrace)?;
        let mut methods = Vec::new();

        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let method_attributes = self.parse_attributes();
            let method_visibility = self.parse_visibility();
            let func = self.parse_function_with(method_attributes, method_visibility)?;
            methods.push(func);
        }

        self.expect(TokenType::RightBrace)?;

        Ok(InterfaceDecl {
            attributes,
            name,
            generics,
            methods,
        })
    }

    // note: parse protocol with associated types, methods, and extensions

}
