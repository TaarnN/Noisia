use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        self.expect_nv(TokenType::Keyword, "if")?;
        self.parse_if_expr_tail()
    }
}
