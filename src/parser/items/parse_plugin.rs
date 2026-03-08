use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_plugin(&mut self, attributes: Vec<String>) -> ParseResult<PluginDecl> {
        self.expect_nv(TokenType::Keyword, "plugin")?;
        let name = self.expect_ident_like()?;
        
        let condition = if self.match_tnv(TokenType::Keyword, "when") {
            Some(self.parse_expression_or_phrase(&[])?)
        } else {
            None
        };
        
        Ok(PluginDecl {
            attributes,
            name: name.lexeme,
            condition,
        })
    }

}
