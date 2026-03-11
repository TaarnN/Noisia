use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_pointer_deref_postfix(
        &mut self,
        base: Expr,
        safe: bool,
    ) -> ParseResult<Expr> {
        let deref = Expr::PointerDeref {
            expr: Box::new(base),
            safe,
        };

        if !matches!(
            self.peek().token_type,
            TokenType::Identifier | TokenType::Keyword
        ) {
            return Ok(deref);
        }

        let field_or_method = self.expect_ident_like()?.lexeme;
        let mut generics = Vec::new();
        if self.peek().token_type == TokenType::Operator && self.peek().lexeme == "<" {
            self.advance();
            generics = self.parse_generic_args()?;
        }

        if self.peek().token_type == TokenType::LeftParen {
            self.advance();
            let args = self.parse_arguments()?;
            self.expect(TokenType::RightParen)?;
            Ok(Expr::MethodCall {
                object: Box::new(deref),
                method: field_or_method,
                generics,
                args,
            })
        } else {
            if !generics.is_empty() {
                return Err(self.error_here("Unexpected generics without method call"));
            }
            Ok(Expr::FieldAccess {
                object: Box::new(deref),
                field: field_or_method,
            })
        }
    }
}
