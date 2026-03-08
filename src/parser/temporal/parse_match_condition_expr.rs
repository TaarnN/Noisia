use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_match_condition_expr(&mut self) -> ParseResult<Expr> {
        let raw = self.parse_tokens_until_short_arrow("match condition")?;
        let mut segments: Vec<Vec<Token>> = Vec::new();
        let mut current = Vec::new();
        let mut paren_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut bracket_depth = 0usize;

        for tok in raw {
            let at_top = paren_depth == 0 && brace_depth == 0 && bracket_depth == 0;
            if at_top && tok.token_type == TokenType::FatArrow && tok.lexeme == "->" {
                if current.is_empty() {
                    return Err(self.error_here("Expected expression before '->'"));
                }
                segments.push(current);
                current = Vec::new();
                continue;
            }

            match tok.token_type {
                TokenType::LeftParen => paren_depth += 1,
                TokenType::RightParen => paren_depth = paren_depth.saturating_sub(1),
                TokenType::LeftBrace => brace_depth += 1,
                TokenType::RightBrace => brace_depth = brace_depth.saturating_sub(1),
                TokenType::LeftBracket => bracket_depth += 1,
                TokenType::RightBracket => bracket_depth = bracket_depth.saturating_sub(1),
                _ => {}
            }
            current.push(tok);
        }

        if current.is_empty() {
            return Err(self.error_here("Expected expression after '->'"));
        }
        segments.push(current);

        let mut segments = segments.into_iter();
        let mut expr =
            self.parse_expression_from_tokens(segments.next().unwrap(), 1, "match condition")?;

        for segment in segments {
            let rhs = self.parse_expression_from_tokens(segment, 1, "match condition")?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: "->".into(),
                right: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    // note: parse debug temporal statement

}
