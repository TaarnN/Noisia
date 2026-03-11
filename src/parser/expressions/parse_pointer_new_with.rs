use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_pointer_new_with(
        &mut self,
        pointer_type: PointerType,
    ) -> ParseResult<Expr> {
        self.expect(TokenType::LeftParen)?;
        let expr = self.parse_expression(0)?;
        self.expect(TokenType::RightParen)?;
        Ok(Expr::PointerNew {
            pointer_type,
            expr: Box::new(expr),
            at: None,
            expires: None,
        })
    }

    // note: parse literals, ids, groups
}
