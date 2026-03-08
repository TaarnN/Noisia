use super::super::*;

impl Parser {
    pub(in crate::parser) fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.idx += 1;
        }
        &self.tokens[self.idx.saturating_sub(1)]
    }

    // note: require token type or fail

}
