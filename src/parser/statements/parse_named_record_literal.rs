use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_named_record_literal(
        &mut self,
        name: &str,
    ) -> ParseResult<Expr> {
        self.expect(TokenType::LeftBrace)?;
        let fields = self.parse_struct_fields()?;
        Ok(Expr::Literal(Literal::Struct {
            name: name.to_string(),
            base: None,
            fields,
        }))
    }

    // note: detect `{ key: value }`-style payload blocks
}
