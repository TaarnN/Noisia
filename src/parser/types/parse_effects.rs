use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_effects(&mut self) -> ParseResult<Vec<Effect>> {
        let mut effects = Vec::new();

        loop {
            let mut name_parts: Vec<String> = Vec::new();

            if self.peek().token_type == TokenType::EffectMarker {
                let marker = self.advance().lexeme.clone();
                let stripped = marker.trim_start_matches('!');
                if !stripped.is_empty() {
                    name_parts.push(stripped.to_string());
                }
            } else if self.match_tnv(TokenType::Operator, "!") {
                // note: consume traditional effect marker operator
            } else {
                break;
            }

            while matches!(
                self.peek().token_type,
                TokenType::Identifier | TokenType::ModulePath | TokenType::Keyword
            ) && !(self.peek().token_type == TokenType::Keyword
                && matches!(
                    self.peek().lexeme.as_str(),
                    "where" | "catch" | "fn" | "let" | "return"
                ))
            {
                name_parts.push(self.advance().lexeme.clone());
            }

            if name_parts.is_empty() {
                return Err(self.error_here("Expected effect name after '!'"));
            }

            let name = name_parts.join(" ");
            let mut params = None;

            if self.match_one(TokenType::LeftParen) {
                params = Some(self.parse_effect_params()?);
                self.expect(TokenType::RightParen)?;
            }

            effects.push(Effect { name, params });
        }

        Ok(effects)
    }

    // note: parse constraint list
}
