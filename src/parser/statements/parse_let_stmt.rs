use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_let_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let mutable = (!self.is_at_end() && self.match_tnv(TokenType::Operator, "~"))
            .then(|| true)
            .unwrap_or(false);

        let pattern = self.parse_pattern()?;
        let mut typ = None;
        if self.match_one(TokenType::Colon) {
            typ = Some(self.parse_type()?);
        }
        let mut expr = None;
        if self.match_tnv(TokenType::Operator, "=") {
            expr = Some(self.parse_expression(0)?);
        }

        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }
        Ok(Stmt::Let {
            pattern,
            mutable,
            typ,
            expr,
        })
    }

    // note: parse try/catch statement

}
