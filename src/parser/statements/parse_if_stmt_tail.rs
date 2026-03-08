use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_if_stmt_tail(&mut self) -> ParseResult<Stmt> {
        let cond = self.parse_expr_with_struct_literal_guard(false)?;
        let then_block = self.parse_block()?;
        let else_block = if self.match_tnv(TokenType::Keyword, "elif") {
            let nested = self.parse_if_stmt_tail()?;
            Some(Block {
                stmts: vec![nested],
            })
        } else if self.match_tnv(TokenType::Keyword, "else") {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Stmt::If {
            cond,
            then_block,
            else_block,
        })
    }

    // note: parse pattern list for for-loops

}
