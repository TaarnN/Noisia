use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_operator_method(
        &mut self,
        attributes: Vec<String>,
        visibility: Visibility,
    ) -> ParseResult<FunctionDecl> {
        let name_tok = self.peek().clone();
        let name = match name_tok.token_type {
            TokenType::Operator | TokenType::Identifier | TokenType::Keyword => {
                self.advance();
                name_tok.lexeme
            }
            _ => {
                return Err(self.error_here("Expected operator name after @operator"));
            }
        };

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

        let mut throws = false;
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
            modifiers: Vec::new(),
            name,
            generics,
            params,
            context_params: Vec::new(),
            ret_type,
            effects,
            throws,
            where_clauses,
            body,
            is_async: false,
        })
    }
}
