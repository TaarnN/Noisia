use super::super::*;

impl Parser {
    pub(in crate::parser) fn is_at_end(&self) -> bool {
        matches!(self.peek().token_type, TokenType::EOF)
    }

    // note: move index and return old token

}
