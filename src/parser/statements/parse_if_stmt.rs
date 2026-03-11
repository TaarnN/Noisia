use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_nv(TokenType::Keyword, "if")?;
        self.parse_if_stmt_tail()
    }
}
