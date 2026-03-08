use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_class(&mut self, attributes: Vec<String>) -> ParseResult<ClassDecl> {
        self.expect_nv(TokenType::Keyword, "class")?;

        let name_tok = self.expect_ident_like()?;
        let name = name_tok.lexeme;

        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        let (extends, mixins, implements) = self.parse_class_inheritance()?;

        self.expect(TokenType::LeftBrace)?;

        let mut fields = Vec::new();
        let mut properties = Vec::new();
        let mut static_inits = Vec::new();
        let mut deinit = None;
        let mut delegates = Vec::new();
        let mut ctors = Vec::new();
        let mut methods = Vec::new();
        let mut friends = Vec::new();

        while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
            let member_attributes = self.parse_attributes();
            let member_visibility = self.parse_visibility();

            if self.is_word("friend")
                && matches!(
                    self.tokens.get(self.idx + 1),
                    Some(t) if t.lexeme == "class"
                )
            {
                self.advance();
                self.expect_word("class")?;
                let friend = self.parse_module_path_string()?;
                friends.push(friend);
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "deinit" {
                self.advance();
                let body = self.parse_block()?;
                if deinit.is_some() {
                    return Err(self.error_here("Only one deinit block is allowed per class"));
                }
                deinit = Some(body);
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "init" {
                let is_static_init = member_attributes.iter().any(|attr| attr == "@static")
                    && self
                        .tokens
                        .get(self.idx + 1)
                        .map(|tok| tok.token_type == TokenType::LeftBrace)
                        .unwrap_or(false);
                if is_static_init {
                    self.advance();
                    let body = self.parse_block()?;
                    static_inits.push(body);
                } else {
                    let ctor = self.parse_constructor(member_attributes, member_visibility)?;
                    ctors.push(ctor);
                }
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

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

            if member_attributes.iter().any(|attr| attr == "@operator") {
                let method = self.parse_operator_method(member_attributes, member_visibility)?;
                methods.push(method);
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
                "Unsupported member in class body: {}",
                self.peek()
            )));
        }

        self.expect(TokenType::RightBrace)?;
        Ok(ClassDecl {
            attributes,
            name,
            generics,
            extends,
            mixins,
            implements,
            friends,
            fields,
            properties,
            static_inits,
            deinit,
            delegates,
            ctors,
            methods,
        })
    }


}
