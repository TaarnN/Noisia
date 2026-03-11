use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_rewind_stmt(&mut self) -> ParseResult<Stmt> {
        let (subject, target, condition, query, else_expr) = self.parse_rewind_parts(false)?;

        if else_expr.is_some() || self.is_word("else") {
            return Err(self
                .error_here("Rewind statement does not support 'else'; use the expression form"));
        }

        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }

        Ok(Stmt::Rewind {
            subject,
            target,
            condition,
            query,
        })
    }

    // note: parse rewind expression with fallback
}
