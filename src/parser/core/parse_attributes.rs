use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_attributes(&mut self) -> Vec<String> {
        let mut attributes = Vec::new();
        while matches!(
            self.peek().token_type,
            TokenType::Attribute | TokenType::ParameterizedAttribute
        ) {
            attributes.push(self.advance().lexeme.clone());
        }
        attributes
    }

    // note: read visibility, default public

}
