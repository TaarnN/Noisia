use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_function_with(
        &mut self,
        attributes: Vec<String>,
        visibility: Visibility,
    ) -> ParseResult<FunctionDecl> {
        let mut modifiers = Vec::new();
        let mut is_async = false;
        let mut throws = false;

        while self.peek().token_type == TokenType::Keyword {
            let modifier = match self.peek().lexeme.as_str() {
                "async" => Some(FunctionModifier::Async),
                "constexpr" => Some(FunctionModifier::Constexpr),
                "comptime" => Some(FunctionModifier::Comptime),
                "scope" => Some(FunctionModifier::Scoped),
                _ => None,
            };
            match modifier {
                Some(FunctionModifier::Async) => {
                    self.advance();
                    is_async = true;
                    modifiers.push(FunctionModifier::Async);
                }
                Some(m) => {
                    self.advance();
                    modifiers.push(m);
                }
                None => break,
            }
        }

        self.expect_nv(TokenType::Keyword, "fn")?;

        if self.match_tnv(TokenType::Operator, "+") {
            modifiers.push(FunctionModifier::Multidispatch);
        }

        let name_tok = self.expect_ident_like()?;
        let name = name_tok.lexeme.clone();

        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        fn parse_param(this: &mut Parser) -> ParseResult<Param> {
            let pattern = this.parse_pattern()?;
            let mut typ = None;
            if this.match_one(TokenType::Colon) {
                typ = Some(this.parse_type()?);
            }
            let mut default = None;
            if this.match_tnv(TokenType::Operator, "=") {
                default = Some(this.parse_expression(0)?);
            }
            Ok(Param {
                pattern,
                typ,
                default,
            })
        }

        self.expect(TokenType::LeftParen)?;
        let params =
            self.parse_comma_separated(|tok| tok.token_type == TokenType::RightParen, parse_param)?;
        self.expect(TokenType::RightParen)?;

        let mut context_params = Vec::new();
        let has_context_params = self.peek().token_type == TokenType::LeftParen
            && self
                .tokens
                .get(self.idx + 1)
                .map_or(false, |tok| {
                    tok.token_type == TokenType::Keyword && tok.lexeme == "using"
                });
        if has_context_params {
            self.expect(TokenType::LeftParen)?;
            self.expect_nv(TokenType::Keyword, "using")?;
            context_params = self.parse_comma_separated(
                |tok| tok.token_type == TokenType::RightParen,
                parse_param,
            )?;
            self.expect(TokenType::RightParen)?;
        }

        if self.match_word("throws") {
            throws = true;
        }

        let ret_type = if self.match_one(TokenType::FatArrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        if !throws && self.match_word("throws") {
            throws = true;
        }

        let effects = if self.peek().token_type == TokenType::EffectMarker
            || (self.peek().token_type == TokenType::Operator && self.peek().lexeme == "!")
        {
            self.parse_effects()?
        } else {
            Vec::new()
        };

        let where_clauses = if self.match_tnv(TokenType::Keyword, "where") {
            self.parse_where_clauses()?
        } else {
            Vec::new()
        };

        let body = if self.match_one(TokenType::ShortArrow) {
            Some(Block {
                stmts: vec![Stmt::Return(Some(self.parse_expression(0)?))],
            })
        } else if self.peek().token_type == TokenType::LeftBrace {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(FunctionDecl {
            attributes,
            visibility,
            modifiers,
            name,
            generics,
            params,
            context_params,
            ret_type,
            effects,
            throws,
            where_clauses,
            body,
            is_async,
        })
    }

    // note: parse generic params

}
