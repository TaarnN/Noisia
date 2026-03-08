use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_branch_expr(&mut self) -> ParseResult<Expr> {
        self.expect_word("branch")?;
        let mut name = None;
        if matches!(
            self.peek().token_type,
            TokenType::StringLiteral | TokenType::MultilineStringLiteral
        ) {
            name = Some(self.parse_string_literal_value()?);
        }
        self.expect_word("from")?;
        let from = self.parse_expression(0)?;
        let body = self.parse_block()?;
        Ok(Expr::Branch {
            name,
            from: Box::new(from),
            body,
        })
    }

    // note: parse inspect statement

}
