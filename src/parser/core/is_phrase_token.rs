use super::super::*;

impl Parser {
    pub(in crate::parser) fn is_phrase_token(tok: &Token) -> bool {
        matches!(
            tok.token_type,
            TokenType::Identifier | TokenType::Keyword | TokenType::ModulePath
        )
    }


}
