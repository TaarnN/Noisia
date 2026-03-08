use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_enum(&mut self, attributes: Vec<String>) -> ParseResult<EnumDecl> {
        self.expect_nv(TokenType::Keyword, "enum")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        self.expect(TokenType::LeftBrace)?;

        let mut variants = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let var_tok = self.expect(TokenType::Identifier)?;
            let var_name = var_tok.lexeme;

            let kind = if self.peek().token_type == TokenType::LeftBrace {
                self.advance();
                let fields = self.parse_comma_separated(
                    |tok| tok.token_type == TokenType::RightBrace,
                    |this| {
                        let field_name = this.expect(TokenType::Identifier)?.lexeme;
                        this.expect(TokenType::Colon)?;
                        let typ = this.parse_type()?;
                        Ok(EnumVariantField {
                            name: Some(field_name),
                            typ,
                        })
                    },
                )?;
                self.expect(TokenType::RightBrace)?;
                VariantKind::Struct(fields)
            } else if self.peek().token_type == TokenType::LeftParen {
                self.advance();
                let fields = self.parse_comma_separated(
                    |tok| tok.token_type == TokenType::RightParen,
                    |this| {
                        let is_named = this.peek().token_type == TokenType::Identifier
                            && matches!(
                                this.tokens.get(this.idx + 1),
                                Some(t) if t.token_type == TokenType::Colon
                            );

                        if is_named {
                            let field_name = this.expect(TokenType::Identifier)?.lexeme;
                            this.expect(TokenType::Colon)?;
                            let typ = this.parse_type()?;
                            Ok(EnumVariantField {
                                name: Some(field_name),
                                typ,
                            })
                        } else {
                            let typ = this.parse_type()?;
                            Ok(EnumVariantField { name: None, typ })
                        }
                    },
                )?;
                self.expect(TokenType::RightParen)?;
                VariantKind::Tuple(fields)
            } else {
                VariantKind::Unit
            };

            variants.push(EnumVariant {
                name: var_name,
                kind,
            });

            if self.peek().token_type == TokenType::RightBrace {
                break;
            }
            self.expect(TokenType::Comma)?;
        }

        self.expect(TokenType::RightBrace)?;
        Ok(EnumDecl {
            attributes,
            name,
            generics,
            variants,
        })
    }

    // note: parse mixin with fields, properties, delegates, and methods

}
