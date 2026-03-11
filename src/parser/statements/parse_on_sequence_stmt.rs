use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_on_sequence_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_nv(TokenType::Keyword, "on")?;
        self.expect_nv(TokenType::Keyword, "sequence")?;

        let target = self.parse_expression(1)?;
        self.expect_nv(TokenType::Operator, "=")?;
        let sequence = self.parse_expression(0)?;

        self.expect(TokenType::ShortArrow)?;
        let body = self.parse_block_or_expr_body()?;

        Ok(Stmt::OnSequence {
            target,
            sequence,
            body,
        })
    }

    // note: Pratt expression parse
}
