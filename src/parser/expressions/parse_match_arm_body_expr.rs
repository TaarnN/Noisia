use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_match_arm_body_expr(&mut self) -> ParseResult<Expr> {
        if self.peek().token_type == TokenType::LeftBrace {
            return Ok(Expr::Block(self.parse_block()?));
        }

        self.parse_match_inline_arm_body_expr()
    }

    // note: parse match expression

}
