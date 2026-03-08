use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_statement(&mut self) -> ParseResult<Stmt> {
        self.parse_stmt_dispatch()
    }
}
