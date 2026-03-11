use super::super::*;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            idx: 0,
            allow_struct_literals: true,
            allow_trailing_closures: true,
        }
    }

    // note: look at current token or fake EOF
}
