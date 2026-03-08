use super::super::*;

impl Parser {
    pub(in crate::parser) fn match_gt(&mut self) -> bool {
        let tok = self.peek().clone();
        if tok.token_type == TokenType::Operator && tok.lexeme == ">" {
            self.advance();
            return true;
        }

        if tok.token_type == TokenType::Operator && tok.lexeme == ">>" {
            self.tokens[self.idx].lexeme = ">".to_string();
            let extra = Token::new(
                TokenType::Operator,
                ">".to_string(),
                tok.line,
                tok.column + 1,
            );
            self.tokens.insert(self.idx + 1, extra);
            self.advance();
            return true;
        }

        false
    }

    // note: require > with >> support

}
