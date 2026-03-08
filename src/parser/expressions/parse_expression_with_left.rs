use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_expression_with_left(&mut self, mut left: Expr, min_prec: u8) -> ParseResult<Expr> {
        loop {
            let op_tok = self.peek().clone();
            let (prec, right_assoc) = match op_precedence(&op_tok) {
                Some((p, ra)) => (p, ra),
                None => break,
            };

            if prec < min_prec {
                break;
            }

            let op_lex = self.advance().lexeme.clone();

            if op_lex == "?" {
                left = Expr::Try(Box::new(left));
                continue;
            }

            let next_min = if right_assoc { prec } else { prec + 1 };

            if op_lex == ".." || op_lex == "..=" {
                let inclusive = op_lex == "..=";
                let end = if self.is_range_end_delimiter() {
                    None
                } else {
                    Some(self.parse_expression(next_min)?)
                };
                let step = if self.match_tnv(TokenType::Keyword, "by") {
                    Some(Box::new(self.parse_expression(0)?))
                } else {
                    None
                };

                left = Expr::Range {
                    start: Some(Box::new(left)),
                    end: end.map(Box::new),
                    inclusive,
                    step,
                };
                continue;
            }

            if op_lex == "??" {
                let right = self.parse_expression(next_min)?;
                left = Expr::Coalesce {
                    left: Box::new(left),
                    right: Box::new(right),
                };
                continue;
            }

            let right = self.parse_expression(next_min)?;

            if op_lex == "|>" {
                left = Expr::Pipeline {
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if op_lex == "~>" {
                left = Expr::PointerPipeline {
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if op_lex == ">>" {
                left = Expr::SelectorPipeline {
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                left = Expr::Binary {
                    left: Box::new(left),
                    op: op_lex,
                    right: Box::new(right),
                };
            }
        }

        Ok(left)
    }

    // note: parse if/elif/else expression

}
