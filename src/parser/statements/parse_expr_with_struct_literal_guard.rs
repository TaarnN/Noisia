use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_expr_with_struct_literal_guard(
        &mut self,
        allow_struct_literal: bool,
    ) -> ParseResult<Expr> {
        let prev = self.allow_struct_literals;
        let prev_trailing = self.allow_trailing_closures;
        self.allow_struct_literals = allow_struct_literal;
        if !allow_struct_literal {
            self.allow_trailing_closures = false;
        }
        let parsed = self.parse_expression(0);
        self.allow_struct_literals = prev;
        self.allow_trailing_closures = prev_trailing;
        parsed
    }

    // note: parse let statement

}
