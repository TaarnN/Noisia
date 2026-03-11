use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_visibility(&mut self) -> Visibility {
        if self.match_tnv(TokenType::Keyword, "public") {
            Visibility::Public
        } else if self.match_tnv(TokenType::Keyword, "private") {
            Visibility::Private
        } else if self.match_tnv(TokenType::Keyword, "protected") {
            Visibility::Protected
        } else if self.match_tnv(TokenType::Keyword, "internal") {
            Visibility::Internal
        } else if self.match_tnv(TokenType::Keyword, "package") {
            Visibility::Package
        } else {
            Visibility::Public
        }
    }
}
