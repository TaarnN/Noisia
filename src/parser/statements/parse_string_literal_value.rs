use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_string_literal_value(&mut self) -> ParseResult<String> {
        let tok = if self.peek().token_type == TokenType::StringLiteral
            || self.peek().token_type == TokenType::MultilineStringLiteral
        {
            self.advance().clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "String literal".into(),
                found: self.peek().clone(),
                idx: self.idx,
            });
        };
        Ok(strip_string_delimiters(&tok.lexeme))
    }

    // note: parse record literal in braces with a synthetic name
}
