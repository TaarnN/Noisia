use super::super::*;

impl Parser {
    pub(in crate::parser) fn parse_pattern_dispatch(&mut self) -> ParseResult<Pattern> {
        let tok = self.peek().clone();
        if tok.token_type == TokenType::Operator && tok.lexeme == "&" {
            self.advance();
            let inner = self.parse_pattern()?;
            let bindings = inner.bindings.clone();
            return Ok(Pattern {
                kind: PatternKind::Ref(Box::new(inner)),
                bindings,
            });
        }
        let (kind, bindings) = match tok.token_type {
            TokenType::LeftParen => {
                self.advance();
                let (patterns, bindings) = self.parse_pattern_list(TokenType::RightParen)?;
                (PatternKind::Tuple(patterns), bindings)
            }
            TokenType::LeftBracket => {
                self.advance();
                let (patterns, bindings) = self.parse_pattern_list(TokenType::RightBracket)?;
                (PatternKind::Array(patterns), bindings)
            }
            TokenType::LeftBrace => {
                self.advance();
                let (fields, bindings) = self.parse_struct_pattern_fields()?;
                (PatternKind::Struct { name: None, fields }, bindings)
            }
            TokenType::Identifier | TokenType::Keyword => {
                let ident = self.advance().lexeme.clone();
                let is_upper = ident.chars().next().map_or(false, |c| c.is_uppercase());
                if ident == "_" {
                    (PatternKind::Wildcard, Vec::new())
                } else if ident == "Nil" {
                    (PatternKind::Nil, Vec::new())
                } else if ident == "None" {
                    (PatternKind::NoneVariant, Vec::new())
                } else if ident == "Some" && self.peek().token_type == TokenType::LeftParen {
                    self.advance();
                    let inner_pat = self.parse_pattern()?;
                    let bindings = inner_pat.bindings.clone();
                    self.expect(TokenType::RightParen)?;
                    (PatternKind::SomeVariant(Box::new(inner_pat)), bindings)
                } else if ident == "Ok" && self.peek().token_type == TokenType::LeftParen {
                    self.advance();
                    let inner_pat = self.parse_pattern()?;
                    let bindings = inner_pat.bindings.clone();
                    self.expect(TokenType::RightParen)?;
                    (PatternKind::OkVariant(Box::new(inner_pat)), bindings)
                } else if ident == "Err" && self.peek().token_type == TokenType::LeftParen {
                    self.advance();
                    let inner_pat = self.parse_pattern()?;
                    let bindings = inner_pat.bindings.clone();
                    self.expect(TokenType::RightParen)?;
                    (PatternKind::ErrVariant(Box::new(inner_pat)), bindings)
                } else if self.peek().token_type == TokenType::LeftBrace {
                    self.advance();
                    let (fields, bindings) = self.parse_struct_pattern_fields()?;
                    if is_upper {
                        let inner = Pattern {
                            kind: PatternKind::Struct { name: None, fields },
                            bindings: bindings.clone(),
                        };
                        (
                            PatternKind::EnumVariant {
                                variant_name: ident,
                                inner_pattern: Some(Box::new(inner)),
                            },
                            bindings,
                        )
                    } else {
                        (
                            PatternKind::Struct {
                                name: Some(ident),
                                fields,
                            },
                            bindings,
                        )
                    }
                } else if self.peek().token_type == TokenType::LeftParen {
                    self.advance();
                    let (patterns, bindings) = self.parse_pattern_list(TokenType::RightParen)?;
                    if patterns.is_empty() {
                        return Err(self.error_here("Empty tuple in variant"));
                    }
                    let inner = if patterns.len() == 1 {
                        Some(Box::new(patterns.into_iter().next().unwrap()))
                    } else {
                        Some(Box::new(Pattern {
                            kind: PatternKind::Tuple(patterns),
                            bindings: bindings.clone(),
                        }))
                    };
                    (
                        PatternKind::EnumVariant {
                            variant_name: ident,
                            inner_pattern: inner,
                        },
                        bindings,
                    )
                } else if is_upper {
                    (
                        PatternKind::EnumVariant {
                            variant_name: ident,
                            inner_pattern: None,
                        },
                        Vec::new(),
                    )
                } else {
                    (PatternKind::Identifier(ident.clone()), vec![ident])
                }
            }
            TokenType::IntLiteral
            | TokenType::FloatLiteral
            | TokenType::StringLiteral
            | TokenType::BoolLiteral => {
                let literal = match self.parse_primary()? {
                    Expr::Literal(l) => l,
                    _ => return Err(self.error_here("Expected literal")),
                };
                (PatternKind::Literal(literal), Vec::new())
            }
            _ => return Err(self.error_here("Expected pattern")),
        };

        let mut total_bindings = bindings.clone();
        let mut or_patterns = vec![Pattern { kind, bindings }];

        while self.match_tnv(TokenType::Operator, "|") {
            let sub_pat = self.parse_pattern()?;
            total_bindings.extend(sub_pat.bindings.clone());
            or_patterns.push(sub_pat);
        }

        if or_patterns.len() > 1 {
            Ok(Pattern {
                kind: PatternKind::Or(or_patterns),
                bindings: total_bindings,
            })
        } else {
            Ok(or_patterns.remove(0))
        }
    }

    // note: parse \ params :> expr
}
