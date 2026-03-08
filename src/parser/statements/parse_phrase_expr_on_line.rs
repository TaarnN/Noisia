use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_phrase_expr_on_line(&mut self, stop_words: &[&str]) -> ParseResult<Expr> {
        let start_line = self.peek().line;
        let mut words: Vec<String> = Vec::new();

        while !self.is_at_end() {
            let tok = self.peek().clone();
            if tok.line != start_line {
                break;
            }
            if matches!(
                tok.token_type,
                TokenType::Semicolon | TokenType::RightBrace | TokenType::EOF
            ) {
                break;
            }
            if stop_words.iter().any(|w| *w == tok.lexeme) {
                break;
            }
            if !Self::is_phrase_token(&tok) {
                break;
            }
            words.push(tok.lexeme.clone());
            self.advance();
        }

        if words.is_empty() {
            return Err(self.error_here("Expected phrase"));
        }

        Ok(Expr::Phrase(words.join(" ")))
    }


}
