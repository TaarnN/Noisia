use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_extension(&mut self, attributes: Vec<String>) -> ParseResult<ExtensionDecl> {
        self.expect_nv(TokenType::Keyword, "extension")?;
        
        let (receiver, target) = if self.match_one(TokenType::LeftParen) {
            // extension (name: Type) form
            let receiver_name = self.expect(TokenType::Identifier)?.lexeme;
            self.expect(TokenType::Colon)?;
            let target_type = self.parse_type()?;
            self.expect(TokenType::RightParen)?;
            (Some(receiver_name), target_type)
        } else {
            // extension Type form
            let target_type = self.parse_type()?;
            (None, target_type)
        };

        let mut methods = Vec::new();
        self.expect(TokenType::LeftBrace)?;
        while !self.match_one(TokenType::RightBrace) && !self.is_at_end() {
            let method_attrs = self.parse_attributes();
            let method_vis = self.parse_visibility();
            let method = self.parse_function_with(method_attrs, method_vis)?;
            methods.push(method);
        }

        Ok(ExtensionDecl {
            attributes,
            receiver,
            target,
            methods,
        })
    }

    // note: parse igm name! { pattern = /.../ expand(m: Match) = Expr }

}
