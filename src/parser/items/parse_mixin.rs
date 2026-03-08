use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_mixin(&mut self, attributes: Vec<String>) -> ParseResult<MixinDecl> {
        self.expect_nv(TokenType::Keyword, "mixin")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        self.expect(TokenType::LeftBrace)?;
        let mut fields = Vec::new();
        let mut properties = Vec::new();
        let mut delegates = Vec::new();
        let mut methods = Vec::new();

        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let member_attributes = self.parse_attributes();
            let member_visibility = self.parse_visibility();

            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "delegate" {
                let delegate = self.parse_class_delegate(member_attributes, member_visibility)?;
                delegates.push(delegate);
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            if self.is_function_start() {
                let method = self.parse_function_with(member_attributes, member_visibility)?;
                methods.push(method);
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "mutable"
                || self.peek().token_type == TokenType::Identifier
            {
                let member = self.parse_class_field_or_property(member_attributes, member_visibility)?;
                match member {
                    FieldOrProperty::Field(field) => fields.push(field),
                    FieldOrProperty::Property(prop) => properties.push(prop),
                }
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            return Err(self.error_here(format!(
                "Unsupported member in mixin body: {}",
                self.peek()
            )));
        }

        self.expect(TokenType::RightBrace)?;

        Ok(MixinDecl {
            attributes,
            name,
            fields,
            properties,
            delegates,
            methods,
        })
    }

    // note: parse trait with methods

}
