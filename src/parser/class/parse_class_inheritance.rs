use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_class_inheritance(
        &mut self,
    ) -> ParseResult<(Vec<TypeRef>, Vec<TypeRef>, Vec<TypeRef>)> {
        let mut extends = Vec::new();
        let mut mixins = Vec::new();
        let mut implements = Vec::new();

        let parse_types = |this: &mut Self| -> ParseResult<Vec<TypeRef>> {
            let mut types = Vec::new();
            types.push(this.parse_type()?);
            while this.match_one(TokenType::Comma) {
                types.push(this.parse_type()?);
            }
            Ok(types)
        };

        loop {
            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "extends" {
                self.advance();
                extends.extend(parse_types(self)?);
                continue;
            }
            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "with" {
                self.advance();
                mixins.extend(parse_types(self)?);
                continue;
            }
            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "implements" {
                self.advance();
                implements.extend(parse_types(self)?);
                continue;
            }
            break;
        }

        Ok((extends, mixins, implements))
    }

    // note: parse class header and body
}
