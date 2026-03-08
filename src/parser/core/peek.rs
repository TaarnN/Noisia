use super::super::*;

impl Parser {
    pub(in crate::parser) fn peek(&self) -> &Token {
        self.tokens.get(self.idx).unwrap_or_else(|| {
            static EOF_TOKEN: Token = Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                line: 0,
                column: 0,
            };
            &EOF_TOKEN
        })
    }

    // note: stop when token is EOF

}
