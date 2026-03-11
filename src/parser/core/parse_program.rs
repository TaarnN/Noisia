use super::super::*;

impl Parser {
    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut items = Vec::new();
        while !self.is_at_end() {
            let item = self.parse_item()?;
            items.push(item);
        }
        Ok(Program { items })
    }

    // note: parse one top item with attrs
}
