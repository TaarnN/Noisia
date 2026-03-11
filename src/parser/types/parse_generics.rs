use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_generics(&mut self) -> ParseResult<Vec<GenericParam>> {
        let mut generics = Vec::new();
        if self.peek().token_type == TokenType::Operator && self.peek().lexeme == ">" {
            self.expect_gt()?;
            return Ok(generics);
        }

        loop {
            let name_tok = self.expect(TokenType::Identifier)?;
            let mut constraints = Vec::new();

            if self.match_one(TokenType::Colon) {
                constraints = self.parse_type_constraints()?;
            }

            generics.push(GenericParam {
                name: name_tok.lexeme,
                constraints,
            });

            if self.match_one(TokenType::Comma) {
                if self.peek().token_type == TokenType::Operator && self.peek().lexeme == ">" {
                    break;
                }
                continue;
            }
            break;
        }

        self.expect_gt()?;
        Ok(generics)
    }

    // note: parse effect list
}
