use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_igm(&mut self, attributes: Vec<String>) -> ParseResult<IGMDecl> {
        self.expect_nv(TokenType::Keyword, "igm")?;
        let name = self.expect_ident_like()?;

        // Parse ! after name
        if !(self.peek().token_type == TokenType::Operator && self.peek().lexeme == "!") {
            return Err(self.error_here("Expected '!' after igm name".to_string()));
        }
        self.advance(); // consume '!'

        self.expect(TokenType::LeftBrace)?;

        // Parse pattern = /regex/
        self.expect_word("pattern")?;
        self.expect_nv(TokenType::Operator, "=")?;
        let pattern = if self.peek().token_type == TokenType::RegexLiteral {
            self.advance().lexeme.clone()
        } else {
            return Err(self.error_here("Expected regex literal".to_string()));
        };

        // Parse expand(m: Match) = Expr
        self.expect_word("expand")?;
        self.expect(TokenType::LeftParen)?;
        let expand_params = self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightParen,
            |this| {
                let param_name = this.expect(TokenType::Identifier)?.lexeme;
                let mut param_type = None;
                if this.match_one(TokenType::Colon) {
                    param_type = Some(this.parse_type()?);
                }
                Ok(Param {
                    pattern: Pattern {
                        kind: PatternKind::Identifier(param_name.clone()),
                        bindings: vec![param_name],
                    },
                    typ: param_type,
                    default: None,
                })
            },
        )?;
        self.expect(TokenType::RightParen)?;

        self.expect_nv(TokenType::Operator, "=")?;
        let expand = Some(self.parse_expression(0)?);

        self.expect(TokenType::RightBrace)?;

        Ok(IGMDecl {
            attributes,
            name: name.lexeme,
            pattern,
            expand_params,
            expand,
        })
    }

    // note: parse plugin name when <expr>
}
