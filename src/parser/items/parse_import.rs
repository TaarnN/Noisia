use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_import(&mut self) -> ParseResult<(String, Option<Vec<String>>)> {
        let path = self.parse_module_path_string()?;
        if self.match_one(TokenType::LeftBrace) {
            let syms = self.parse_comma_separated(
                |tok| tok.token_type == TokenType::RightBrace,
                |this| Ok(this.expect(TokenType::Identifier)?.lexeme),
            )?;
            self.expect(TokenType::RightBrace)?;
            Ok((path, Some(syms)))
        } else {
            Ok((path, None))
        }
    }

    // note: parse fn with given attrs+vis
}
