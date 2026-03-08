use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_type_constraints(&mut self) -> ParseResult<Vec<TypeConstraint>> {
        let mut constraints = Vec::new();

        while !self.is_at_end() {
            let tok = self.peek().clone();

            match tok.token_type {
                TokenType::Identifier => {
                    let ident = self.advance().lexeme.clone();

                    if self.match_tnv(TokenType::Operator, "=") {
                        let type_ref = self.parse_type()?;
                        constraints.push(TypeConstraint::TypeEq(ident, type_ref.name));
                    } else if self.match_one(TokenType::Colon) {
                        let trait_name = self.expect(TokenType::Identifier)?.lexeme;
                        constraints.push(TypeConstraint::TraitBound(format!(
                            "{}:{}",
                            ident, trait_name
                        )));
                    } else if self.match_tnv(TokenType::Keyword, "subtype") {
                        self.expect_nv(TokenType::Keyword, "of")?;
                        let supertype = self.parse_type()?;
                        constraints.push(TypeConstraint::SubtypeOf(supertype.name));
                    } else {
                        constraints.push(TypeConstraint::TraitBound(ident));
                    }
                }
                TokenType::Keyword if tok.lexeme == "lifetime" => {
                    self.advance();
                    let lifetime = self.expect(TokenType::Identifier)?.lexeme;
                    constraints.push(TypeConstraint::LifetimeBound(lifetime));
                }
                _ => break,
            }

            if !self.match_tnv(TokenType::Operator, "+") {
                break;
            }
        }

        Ok(constraints)
    }

    // note: parse effect param types

}
