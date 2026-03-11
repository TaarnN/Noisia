use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_struct_pattern_fields(
        &mut self,
    ) -> ParseResult<(Vec<(String, Pattern)>, Vec<String>)> {
        let mut fields = Vec::new();
        let mut bindings = Vec::new();
        while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
            let field_name = self.expect_ident_like()?.lexeme;
            if self.match_one(TokenType::Colon) {
                let field_pat = self.parse_pattern()?;
                bindings.extend(field_pat.bindings.clone());
                fields.push((field_name, field_pat));
            } else {
                let field_pat = Pattern {
                    kind: PatternKind::Identifier(field_name.clone()),
                    bindings: vec![field_name.clone()],
                };
                bindings.push(field_name.clone());
                fields.push((field_name, field_pat));
            }

            if self.match_one(TokenType::Comma) {
                if self.peek().token_type == TokenType::RightBrace {
                    break;
                }
                continue;
            } else {
                break;
            }
        }
        self.expect(TokenType::RightBrace)?;
        Ok((fields, bindings))
    }

    // note: parse pattern and bindings
}
