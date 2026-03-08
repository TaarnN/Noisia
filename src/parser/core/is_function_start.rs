use super::super::*;

impl Parser {
    pub(in crate::parser) fn is_function_start(&self) -> bool {
        let mut i = self.idx;
        while let Some(tok) = self.tokens.get(i) {
            if tok.token_type == TokenType::Keyword && Self::is_function_modifier_keyword(&tok.lexeme)
            {
                i += 1;
                continue;
            }
            return tok.token_type == TokenType::Keyword && tok.lexeme == "fn";
        }
        false
    }


}
