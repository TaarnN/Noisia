use super::super::*;

impl Parser {
    pub(in crate::parser) fn should_parse_phrase_on_line(&self, stop_words: &[&str]) -> bool {
        let tok = self.peek();
        if !Self::is_phrase_token(tok) {
            return false;
        }
        if stop_words.iter().any(|w| *w == tok.lexeme) {
            return false;
        }

        let next = match self.tokens.get(self.idx + 1) {
            Some(n) => n,
            None => return false,
        };

        if next.line != tok.line {
            return false;
        }
        if stop_words.iter().any(|w| *w == next.lexeme) {
            return false;
        }
        Self::is_phrase_token(next)
    }


}
