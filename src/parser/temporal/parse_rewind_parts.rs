use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_rewind_parts(
        &mut self,
        allow_else: bool,
    ) -> ParseResult<(Option<Expr>, Option<Expr>, Option<Expr>, Option<Block>, Option<Expr>)> {
        let rewind_tok = self.expect_word("rewind")?;
        let rewind_line = rewind_tok.line;

        let stop_words = ["to", "if", "where", "else"];
        let mut target = None;
        let mut subject = None;
        let mut condition = None;
        let mut query = None;
        let mut else_expr = None;

        if self.match_word("to") {
            target = Some(self.parse_expression_or_phrase(&["if", "where", "else"])?);
        } else if !matches!(
            self.peek().token_type,
            TokenType::Semicolon | TokenType::RightBrace | TokenType::EOF
        ) && !self.is_word("if")
            && !self.is_word("where")
            && !self.is_word("else")
            && self.peek().line == rewind_line
        {
            subject = Some(self.parse_expression_or_phrase(&stop_words)?);
            if self.match_word("to") {
                target = Some(self.parse_expression_or_phrase(&["if", "where", "else"])?);
            }
        }

        if self.match_word("if") {
            condition = Some(self.parse_expression(0)?);
        }

        if self.match_word("where") {
            query = Some(self.parse_block()?);
        }

        if allow_else && self.match_word("else") {
            else_expr = Some(self.parse_expression(0)?);
        }

        Ok((subject, target, condition, query, else_expr))
    }

    // note: parse checkpoint statement

}
