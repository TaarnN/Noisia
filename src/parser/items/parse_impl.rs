use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_impl(&mut self, attributes: Vec<String>) -> ParseResult<ImplDecl> {
        self.expect_nv(TokenType::Keyword, "impl")?;

        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        let first_type = self.parse_type()?;

        let (trait_name, target) = if self.match_tnv(TokenType::Keyword, "for") {
            let trait_name = Some(first_type.name.clone());
            let target = self.parse_type()?;
            (trait_name, target)
        } else {
            (None, first_type)
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

        Ok(ImplDecl {
            attributes,
            generics,
            trait_name,
            target,
            methods,
        })
    }

    // note: parse block statements

}
