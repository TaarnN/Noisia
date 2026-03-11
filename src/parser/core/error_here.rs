use super::super::*;

impl Parser {
    pub(in crate::parser) fn error_here(&self, message: impl Into<String>) -> ParseError {
        let tok = self.peek();
        ParseError::Generic {
            message: message.into(),
            line: tok.line,
            column: tok.column,
        }
    }

    // note: accept identifier or keyword as name
}
