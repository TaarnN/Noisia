use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_class_delegate(
        &mut self,
        attributes: Vec<String>,
        vis: Visibility,
    ) -> ParseResult<ClassDelegateDecl> {
        self.expect_nv(TokenType::Keyword, "delegate")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        self.expect_word("to")?;
        let target = self.parse_expression(0)?;
        Ok(ClassDelegateDecl {
            attributes,
            vis,
            name: name_tok.lexeme,
            target,
        })
    }
}
