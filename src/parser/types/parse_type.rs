use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_type(&mut self) -> ParseResult<TypeRef> {
        let mut name = String::new();
        let mut generics = Vec::new();
        let mut nullable = false;
        let mut pointer_type = None;
        let mut allow_generics = true;

        let mut tok = self.peek().clone();
        if tok.token_type == TokenType::Operator {
            pointer_type = match tok.lexeme.as_str() {
                "~" => Some(PointerType::RawPointer),
                "@" => Some(PointerType::ManagedPointer),
                "&" => Some(PointerType::WeakPointer),
                "+" => Some(PointerType::SharedPointer),
                _ => None,
            };
            self.advance();
        }

        tok = self.peek().clone();
        match tok.token_type {
            TokenType::LeftBracket => {
                self.advance();
                if self.match_one(TokenType::Colon) {
                    self.expect(TokenType::RightBracket)?;
                    name = "[:]".to_string();
                    allow_generics = false;
                } else {
                    let first = self.parse_type()?;
                    if self.match_one(TokenType::Colon) {
                        let second = self.parse_type()?;
                        self.expect(TokenType::RightBracket)?;
                        name = "[:]".to_string();
                        generics = vec![first, second];
                    } else {
                        self.expect(TokenType::RightBracket)?;
                        name = "[]".to_string();
                        generics = vec![first];
                    }
                    allow_generics = false;
                }
            }
            TokenType::LeftParen => {
                self.advance();
                let params = self.parse_comma_separated(
                    |tok| tok.token_type == TokenType::RightParen,
                    |this| this.parse_type(),
                )?;
                self.expect(TokenType::RightParen)?;
                self.expect(TokenType::FatArrow)?;
                let ret = self.parse_type()?;
                name = "fn".to_string();
                generics = params;
                generics.push(ret);
                allow_generics = false;
            }
            TokenType::Identifier | TokenType::Keyword | TokenType::ModulePath => {
                name = tok.lexeme.clone();
                self.advance();
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "Type identifier".into(),
                    found: tok,
                    idx: self.idx,
                })
            }
        }

        if allow_generics && self.match_tnv(TokenType::Operator, "<") {
            generics = self.parse_generic_args()?;
        }

        if self.match_tnv(TokenType::Operator, "?") {
            nullable = true;
        }

        Ok(TypeRef {
            name,
            generics,
            nullable,
            pointer_type,
        })
    }

    // note: parse list of patterns

}
