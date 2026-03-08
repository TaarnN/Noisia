use super::super::*;

impl Parser {
    pub(in crate::parser) fn looks_like_trailing_closure_with_params(&self) -> bool {
        if self.peek().token_type != TokenType::LeftBrace {
            return false;
        }

        let mut i = self.idx + 1;
        match self.tokens.get(i) {
            Some(tok)
                if (tok.token_type == TokenType::Identifier || tok.token_type == TokenType::Keyword)
                    && tok.lexeme != "in" =>
            {
                {}
            }
            _ => return false,
        }
        i += 1;

        loop {
            match self.tokens.get(i) {
                Some(tok) if tok.token_type == TokenType::Comma => {
                    i += 1;
                    match self.tokens.get(i) {
                        Some(next)
                            if next.token_type == TokenType::Identifier
                                || next.token_type == TokenType::Keyword => {}
                        _ => return false,
                    }
                    i += 1;
                }
                Some(tok)
                    if (tok.token_type == TokenType::Keyword
                        || tok.token_type == TokenType::Identifier)
                        && tok.lexeme == "in" =>
                {
                    return true
                }
                _ => return false,
            }
        }
    }


}
