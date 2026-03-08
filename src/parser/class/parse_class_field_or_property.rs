use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_class_field_or_property(
        &mut self,
        attributes: Vec<String>,
        vis: Visibility,
    ) -> ParseResult<FieldOrProperty> {
        let mutable = self.match_tnv(TokenType::Keyword, "mutable");
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme;

        if self.match_one(TokenType::ShortArrow) {
            let value = self.parse_expression(0)?;
            return Ok(FieldOrProperty::Property(ClassPropertyDecl {
                attributes,
                vis,
                name,
                typ: None,
                value: Some(value),
                getter: None,
                setter_param: None,
                setter_body: None,
            }));
        }

        if self.peek().token_type == TokenType::LeftBrace {
            let (getter, setter_param, setter_body) = self.parse_property_accessors()?;
            return Ok(FieldOrProperty::Property(ClassPropertyDecl {
                attributes,
                vis,
                name,
                typ: None,
                value: None,
                getter,
                setter_param,
                setter_body,
            }));
        }

        self.expect(TokenType::Colon)?;
        let typ = self.parse_type()?;

        if self.match_one(TokenType::ShortArrow) {
            let value = self.parse_expression(0)?;
            return Ok(FieldOrProperty::Property(ClassPropertyDecl {
                attributes,
                vis,
                name,
                typ: Some(typ),
                value: Some(value),
                getter: None,
                setter_param: None,
                setter_body: None,
            }));
        }

        if self.peek().token_type == TokenType::LeftBrace {
            let (getter, setter_param, setter_body) = self.parse_property_accessors()?;
            return Ok(FieldOrProperty::Property(ClassPropertyDecl {
                attributes,
                vis,
                name,
                typ: Some(typ),
                value: None,
                getter,
                setter_param,
                setter_body,
            }));
        }

        let value = if self.match_tnv(TokenType::Operator, "=") {
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        Ok(FieldOrProperty::Field(ClassFieldDecl {
            attributes,
            vis,
            mutable,
            name,
            typ,
            value,
        }))
    }


}
