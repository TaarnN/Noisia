use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_rewind_expr_relaxed(&mut self) -> ParseResult<Expr> {
        let (subject, target, condition, query, else_expr) = self.parse_rewind_parts(true)?;
        Ok(Expr::Rewind {
            subject: subject.map(Box::new),
            target: target.map(Box::new),
            condition: condition.map(Box::new),
            query,
            else_expr: else_expr.map(Box::new),
        })
    }


}
