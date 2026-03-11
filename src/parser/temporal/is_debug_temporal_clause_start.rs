use super::super::*;

impl Parser {
    pub(in crate::parser) fn is_debug_temporal_clause_start(&self) -> bool {
        self.is_word("breakpoint") || self.is_word("trace") || self.is_word("analyze")
    }

    // note: collect one raw debug temporal clause
}
