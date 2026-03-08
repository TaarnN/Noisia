use super::super::*;

impl Parser {
    pub(in crate::parser) fn match_word(&mut self, v: &str) -> bool {
        if self.is_word(v) {
            self.advance();
            true
        } else {
            false
        }
    }

    // note: detect checkpoint statement start

}
