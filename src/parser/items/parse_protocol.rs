use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_protocol(&mut self, attributes: Vec<String>) -> ParseResult<ProtocolDecl> {
        self.expect_nv(TokenType::Keyword, "protocol")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        self.expect(TokenType::LeftBrace)?;
        let mut associated_types = Vec::new();
        let mut methods = Vec::new();
        let mut extensions = Vec::new();

        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let attrs = self.parse_attributes();

            if self.is_word("type") {
                self.advance();
                let type_name = self.expect(TokenType::Identifier)?.lexeme;
                let mut name = type_name;
                if self.match_tnv(TokenType::Operator, "<") {
                    let params = self.parse_generics()?;
                    if !params.is_empty() {
                        let list = params
                            .iter()
                            .map(|p| p.name.as_str())
                            .collect::<Vec<_>>()
                            .join(", ");
                        name = format!("{}<{}>", name, list);
                    }
                }
                associated_types.push(name);
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "extension" {
                self.advance();
                self.expect(TokenType::LeftBrace)?;
                while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
                    let method_attrs = self.parse_attributes();
                    let method_vis = self.parse_visibility();
                    let method = self.parse_function_with(method_attrs, method_vis)?;
                    extensions.push(method);
                }
                self.expect(TokenType::RightBrace)?;
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            let method_visibility = self.parse_visibility();
            let func = self.parse_function_with(attrs, method_visibility)?;
            methods.push(func);
        }

        self.expect(TokenType::RightBrace)?;

        Ok(ProtocolDecl {
            attributes,
            name,
            generics,
            associated_types,
            methods,
            extensions,
        })
    }

    // note: parse impl block

}
