use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_comma_separated<T, F, E>(
        &mut self,
        mut is_end: E,
        mut parse_item: F,
    ) -> ParseResult<Vec<T>>
    where
        F: FnMut(&mut Self) -> ParseResult<T>,
        E: FnMut(&Token) -> bool,
    {
        let mut items = Vec::new();
        while !self.is_at_end() && !is_end(self.peek()) {
            let item = parse_item(self)?;
            items.push(item);
            if self.match_one(TokenType::Comma) {
                if is_end(self.peek()) {
                    break;
                }
                continue;
            } else {
                break;
            }
        }
        Ok(items)
    }

    // note: parse whole file into items

}
