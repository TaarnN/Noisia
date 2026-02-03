#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(dead_code)]

use crate::ptypes::*;
use crate::style::Style;
use crate::tokenizer::{Token, TokenType};
use std::fmt;

impl fmt::Display for Item {
    // note: print item in debug style
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::ModuleDecl(inner) => write!(f, "{:#?}", inner),
            Item::Import(inner) => write!(f, "{:#?}", inner),
            Item::Function(inner) => write!(f, "{:#?}", inner),
            Item::Struct(inner) => write!(f, "{:#?}", inner),
            Item::Enum(inner) => write!(f, "{:#?}", inner),
            Item::Trait(inner) => write!(f, "{:#?}", inner),
            Item::Impl(inner) => write!(f, "{:#?}", inner),
            Item::Class(inner) => write!(f, "{:#?}", inner),
        }
    }
}

impl Item {
    pub fn pretty(&self, style: &Style) -> String {
        let raw = match self {
            Item::ModuleDecl(inner) => format!("{:#?}", inner),
            Item::Import(inner) => format!("{:#?}", inner),
            Item::Function(inner) => format!("{:#?}", inner),
            Item::Struct(inner) => format!("{:#?}", inner),
            Item::Enum(inner) => format!("{:#?}", inner),
            Item::Trait(inner) => format!("{:#?}", inner),
            Item::Impl(inner) => format!("{:#?}", inner),
            Item::Class(inner) => format!("{:#?}", inner),
        };

        colorize_ast_debug(style, &raw)
    }
}

fn colorize_ast_debug(style: &Style, input: &str) -> String {
    let mut out = String::with_capacity(input.len() + input.len() / 4);
    for (idx, line) in input.lines().enumerate() {
        if idx > 0 {
            out.push('\n');
        }

        let (indent, trimmed) = split_indent(line);
        if trimmed.is_empty() {
            out.push_str(line);
            continue;
        }

        if starts_with_closer(trimmed) {
            out.push_str(indent);
            out.push_str(&style.dim(trimmed));
            continue;
        }

        if let Some(colon_idx) = trimmed.find(':') {
            let field = &trimmed[..colon_idx];
            let rest = &trimmed[colon_idx + 1..];
            out.push_str(indent);
            out.push_str(&style.paint(field, &["1", "36"]));
            out.push_str(&style.dim(":"));
            out.push_str(rest);
            continue;
        }

        if let Some(brace_idx) = trimmed.find('{') {
            let (before_raw, after) = trimmed.split_at(brace_idx);
            let before_trimmed = before_raw.trim_end();
            if before_trimmed.is_empty() {
                out.push_str(indent);
                out.push_str(&style.dim(trimmed));
            } else {
                let gap = &before_raw[before_trimmed.len()..];
                let after_full = format!("{}{}", gap, after);
                out.push_str(indent);
                out.push_str(&style.paint(before_trimmed, &["1", "35"]));
                out.push_str(&style.dim(&after_full));
            }
            continue;
        }

        if let Some(paren_idx) = trimmed.find('(') {
            let (before_raw, after) = trimmed.split_at(paren_idx);
            if !before_raw.trim().is_empty() {
                out.push_str(indent);
                out.push_str(&style.paint(before_raw.trim_end(), &["1", "35"]));
                out.push_str(&style.dim(after));
                continue;
            }
        }

        out.push_str(indent);
        out.push_str(trimmed);
    }

    out
}

fn split_indent(line: &str) -> (&str, &str) {
    let mut idx = 0;
    for (i, ch) in line.char_indices() {
        if !ch.is_whitespace() {
            idx = i;
            break;
        }
        idx = i + ch.len_utf8();
    }
    line.split_at(idx)
}

fn starts_with_closer(trimmed: &str) -> bool {
    matches!(
        trimmed.chars().next(),
        Some('}') | Some(']') | Some(')')
    )
}

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    // note: make parser from token list
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, idx: 0 }
    }

    // note: look at current token or fake EOF
    fn peek(&self) -> &Token {
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
    fn is_at_end(&self) -> bool {
        matches!(self.peek().token_type, TokenType::EOF)
    }

    // note: move index and return old token
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.idx += 1;
        }
        &self.tokens[self.idx.saturating_sub(1)]
    }

    // note: require token type or fail
    fn expect(&mut self, expected: TokenType) -> ParseResult<Token> {
        let tok = self.peek().clone();
        if tok.token_type == expected {
            Ok(self.advance().clone())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", expected),
                found: tok,
                idx: self.idx,
            })
        }
    }

    // note: create a generic parse error at current token
    fn error_here(&self, message: impl Into<String>) -> ParseError {
        let tok = self.peek();
        ParseError::Generic {
            message: message.into(),
            line: tok.line,
            column: tok.column,
        }
    }

    // note: accept identifier or keyword as name
    fn expect_ident_like(&mut self) -> ParseResult<Token> {
        let tok = self.peek().clone();
        match tok.token_type {
            TokenType::Identifier | TokenType::Keyword => Ok(self.advance().clone()),
            _ => Err(ParseError::UnexpectedToken {
                expected: "Identifier".into(),
                found: tok,
                idx: self.idx,
            }),
        }
    }

    // note: require token type with exact text
    fn expect_nv(&mut self, expected: TokenType, v: &str) -> ParseResult<Token> {
        let tok = self.peek().clone();
        if tok.token_type == expected && tok.lexeme == v {
            Ok(self.advance().clone())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", expected),
                found: tok,
                idx: self.idx,
            })
        }
    }

    // note: eat token by type if present
    fn match_one(&mut self, t: TokenType) -> bool {
        if self.peek().token_type == t {
            self.advance();
            true
        } else {
            false
        }
    }

    // note: eat token by type and text
    fn match_tnv(&mut self, t: TokenType, ch: &str) -> bool {
        let token = self.peek();
        if (token.token_type == t) && (token.lexeme == ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    // note: handle > or split >> for generics
    fn match_gt(&mut self) -> bool {
        let tok = self.peek().clone();
        if tok.token_type == TokenType::Operator && tok.lexeme == ">" {
            self.advance();
            return true;
        }

        if tok.token_type == TokenType::Operator && tok.lexeme == ">>" {
            self.tokens[self.idx].lexeme = ">".to_string();
            let extra = Token::new(
                TokenType::Operator,
                ">".to_string(),
                tok.line,
                tok.column + 1,
            );
            self.tokens.insert(self.idx + 1, extra);
            self.advance();
            return true;
        }

        false
    }

    // note: require > with >> support
    fn expect_gt(&mut self) -> ParseResult<Token> {
        if self.match_gt() {
            Ok(self.tokens[self.idx.saturating_sub(1)].clone())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "Operator(>)".into(),
                found: self.peek().clone(),
                idx: self.idx,
            })
        }
    }

    // note: read all @attrs into list
    fn parse_attributes(&mut self) -> Vec<String> {
        let mut attributes = Vec::new();
        while matches!(
            self.peek().token_type,
            TokenType::Attribute | TokenType::ParameterizedAttribute
        ) {
            attributes.push(self.advance().lexeme.clone());
        }
        attributes
    }

    // note: read visibility, default public
    fn parse_visibility(&mut self) -> Visibility {
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

    // note: parse comma list until end
    fn parse_comma_separated<T, F, E>(
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
    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut items = Vec::new();
        while !self.is_at_end() {
            let item = self.parse_item()?;
            items.push(item);
        }
        Ok(Program { items })
    }

    // note: parse one top item with attrs
    fn parse_item(&mut self) -> ParseResult<Item> {
        let attributes = self.parse_attributes();

        let mut visibility = None;
        if self.peek().token_type == TokenType::Keyword {
            let kw = self.peek().lexeme.as_str();
            if matches!(
                kw,
                "public" | "private" | "protected" | "internal" | "package"
            ) {
                visibility = Some(self.parse_visibility());
            }
        }

        if self.peek().token_type != TokenType::Keyword {
            return Err(self.error_here(format!(
                "Expected keyword after attributes, found {}",
                self.peek()
            )));
        }

        let kw = self.peek().lexeme.clone();
        if visibility.is_some() && !matches!(kw.as_str(), "fn" | "async") {
            return Err(self.error_here(format!(
                "Visibility modifier not allowed before {}",
                kw
            )));
        }

        match kw.as_str() {
            "module" => {
                self.advance();
                let path = self.parse_module_path_string()?;
                Ok(Item::ModuleDecl(ModuleDecl { attributes, path }))
            }
            "import" => {
                self.advance();
                let (path, symbols) = self.parse_import()?;
                Ok(Item::Import(ImportDecl {
                    attributes,
                    path,
                    symbols,
                }))
            }
            "fn" | "async" => {
                let vis = visibility.unwrap_or(Visibility::Public);
                let func = self.parse_function_with(attributes, vis)?;
                Ok(Item::Function(func))
            }
            "struct" => {
                let s = self.parse_struct(attributes)?;
                Ok(Item::Struct(s))
            }
            "enum" => {
                let e = self.parse_enum(attributes)?;
                Ok(Item::Enum(e))
            }
            "trait" => {
                let t = self.parse_trait(attributes)?;
                Ok(Item::Trait(t))
            }
            "impl" => {
                let i = self.parse_impl(attributes)?;
                Ok(Item::Impl(i))
            }
            "class" => {
                let c = self.parse_class(attributes)?;
                Ok(Item::Class(c))
            }
            _ => Err(self.error_here(format!(
                "Unsupported top-level keyword: {}",
                kw
            ))),
        }
    }

    // note: read extends/with/implements
    fn parse_class_inheritance(
        &mut self,
    ) -> ParseResult<(Vec<TypeRef>, Vec<TypeRef>, Vec<TypeRef>)> {
        let mut extends = Vec::new();
        let mut mixins = Vec::new();
        let mut implements = Vec::new();

        let parse_types = |this: &mut Self| -> ParseResult<Vec<TypeRef>> {
            let mut types = Vec::new();
            types.push(this.parse_type()?);
            while this.match_one(TokenType::Comma) {
                types.push(this.parse_type()?);
            }
            Ok(types)
        };

        loop {
            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "extends" {
                self.advance();
                extends.extend(parse_types(self)?);
                continue;
            }
            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "with" {
                self.advance();
                mixins.extend(parse_types(self)?);
                continue;
            }
            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "implements" {
                self.advance();
                implements.extend(parse_types(self)?);
                continue;
            }
            break;
        }

        Ok((extends, mixins, implements))
    }

    // note: parse class header and body
    fn parse_class(&mut self, attributes: Vec<String>) -> ParseResult<ClassDecl> {
        self.expect_nv(TokenType::Keyword, "class")?;

        let name_tok = self.expect_ident_like()?;
        let name = name_tok.lexeme;

        let (extends, mixins, implements) = self.parse_class_inheritance()?;

        self.expect(TokenType::LeftBrace)?;

        let mut fields = Vec::new();
        let mut ctors = Vec::new();
        let mut methods = Vec::new();

        while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
            let member_attributes = self.parse_attributes();
            let member_visibility = self.parse_visibility();

            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "init" {
                let ctor = self.parse_constructor(member_attributes, member_visibility)?;
                ctors.push(ctor);
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            if self.peek().token_type == TokenType::Keyword
                && (self.peek().lexeme == "fn" || self.peek().lexeme == "async")
            {
                let method = self.parse_function_with(member_attributes, member_visibility)?;
                methods.push(method);
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "mutable"
                || self.peek().token_type == TokenType::Identifier
            {
                let mutable = self.match_tnv(TokenType::Keyword, "mutable");
                let name_tok = self.expect(TokenType::Identifier)?;
                self.expect(TokenType::Colon)?;
                let typ = self.parse_type()?;

                let value = if self.match_tnv(TokenType::Operator, "=") {
                    Some(self.parse_expression(0)?)
                } else {
                    None
                };

                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }

                fields.push(ClassFieldDecl {
                    attributes: member_attributes,
                    vis: member_visibility,
                    mutable,
                    name: name_tok.lexeme,
                    typ,
                    value,
                });
                continue;
            }

            return Err(self.error_here(format!(
                "Unsupported member in class body: {}",
                self.peek()
            )));
        }

        self.expect(TokenType::RightBrace)?;
        Ok(ClassDecl {
            attributes,
            name,
            extends,
            mixins,
            implements,
            fields,
            ctors,
            methods,
        })
    }

    // note: parse init with params and block
    fn parse_constructor(
        &mut self,
        attributes: Vec<String>,
        visibility: Visibility,
    ) -> ParseResult<ConstructorDecl> {
        self.expect_nv(TokenType::Keyword, "init")?;

        let ctor_name = if self.match_one(TokenType::Dot) {
            Some(self.expect(TokenType::Identifier)?.lexeme)
        } else {
            None
        };

        self.expect(TokenType::LeftParen)?;
        let params = self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightParen,
            |this| {
                let pattern = this.parse_pattern()?;
                let mut typ = None;
                if this.match_one(TokenType::Colon) {
                    typ = Some(this.parse_type()?);
                }
                let mut default = None;
                if this.match_tnv(TokenType::Operator, "=") {
                    default = Some(this.parse_expression(0)?);
                }
                Ok(Param {
                    pattern,
                    typ,
                    default,
                })
            },
        )?;
        self.expect(TokenType::RightParen)?;

        if self.peek().token_type != TokenType::LeftBrace {
            return Err(self.error_here("Constructors must have a block body"));
        }
        let body = self.parse_block()?;

        Ok(ConstructorDecl {
            attributes,
            visibility,
            name: ctor_name,
            params,
            body,
        })
    }

    // note: read ModulePath or ident::ident
    fn parse_module_path_string(&mut self) -> ParseResult<String> {
        let tok = self.peek().clone();
        match tok.token_type {
            TokenType::ModulePath => {
                self.advance();
                Ok(tok.lexeme)
            }
            TokenType::Identifier => {
                let mut s = tok.lexeme;
                self.advance();
                while self.peek().token_type == TokenType::DoubleColon {
                    self.advance();
                    let next = self.expect(TokenType::Identifier)?;
                    s.push_str("::");
                    s.push_str(&next.lexeme);
                }
                Ok(s)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "ModulePath or Identifier".into(),
                found: tok,
                idx: self.idx,
            }),
        }
    }

    // note: parse import path and symbols
    fn parse_import(&mut self) -> ParseResult<(String, Option<Vec<String>>)> {
        let path = self.parse_module_path_string()?;
        if self.match_one(TokenType::LeftBrace) {
            let syms = self.parse_comma_separated(
                |tok| tok.token_type == TokenType::RightBrace,
                |this| Ok(this.expect(TokenType::Identifier)?.lexeme),
            )?;
            self.expect(TokenType::RightBrace)?;
            Ok((path, Some(syms)))
        } else {
            Ok((path, None))
        }
    }

    // note: parse fn with given attrs+vis
    fn parse_function_with(
        &mut self,
        attributes: Vec<String>,
        visibility: Visibility,
    ) -> ParseResult<FunctionDecl> {
        let mut modifiers = Vec::new();
        let mut is_async = false;

        if self.match_tnv(TokenType::Keyword, "async") {
            modifiers.push(FunctionModifier::Async);
            is_async = true;
        }

        self.expect_nv(TokenType::Keyword, "fn")?;

        if self.match_tnv(TokenType::Operator, "+") {
            modifiers.push(FunctionModifier::Multidispatch);
        }

        let name_tok = self.expect_ident_like()?;
        let name = name_tok.lexeme.clone();

        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        self.expect(TokenType::LeftParen)?;
        let params = self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightParen,
            |this| {
                let pattern = this.parse_pattern()?;
                let mut typ = None;
                if this.match_one(TokenType::Colon) {
                    typ = Some(this.parse_type()?);
                }
                let mut default = None;
                if this.match_tnv(TokenType::Operator, "=") {
                    default = Some(this.parse_expression(0)?);
                }
                Ok(Param {
                    pattern,
                    typ,
                    default,
                })
            },
        )?;
        self.expect(TokenType::RightParen)?;

        let ret_type = if self.match_one(TokenType::FatArrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let effects = if self.match_one(TokenType::EffectMarker) {
            self.parse_effects()?
        } else {
            Vec::new()
        };

        let where_clauses = if self.match_tnv(TokenType::Keyword, "where") {
            self.parse_where_clauses()?
        } else {
            Vec::new()
        };

        let body = if self.match_one(TokenType::ShortArrow) {
            Some(Block {
                stmts: vec![Stmt::Return(Some(self.parse_expression(0)?))],
            })
        } else if self.peek().token_type == TokenType::LeftBrace {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(FunctionDecl {
            attributes,
            visibility,
            modifiers,
            name,
            generics,
            params,
            ret_type,
            effects,
            where_clauses,
            body,
            is_async,
        })
    }

    // note: parse generic params
    fn parse_generics(&mut self) -> ParseResult<Vec<GenericParam>> {
        let mut generics = Vec::new();
        if self.peek().token_type == TokenType::Operator && self.peek().lexeme == ">" {
            self.expect_gt()?;
            return Ok(generics);
        }

        loop {
            let name_tok = self.expect(TokenType::Identifier)?;
            let mut constraints = Vec::new();

            if self.match_one(TokenType::Colon) {
                constraints = self.parse_type_constraints()?;
            }

            generics.push(GenericParam {
                name: name_tok.lexeme,
                constraints,
            });

            if self.match_one(TokenType::Comma) {
                if self.peek().token_type == TokenType::Operator && self.peek().lexeme == ">" {
                    break;
                }
                continue;
            }
            break;
        }

        self.expect_gt()?;
        Ok(generics)
    }

    // note: parse effect list
    fn parse_effects(&mut self) -> ParseResult<Vec<Effect>> {
        let mut effects = Vec::new();

        while self.peek().token_type == TokenType::Identifier {
            let name_tok = self.expect(TokenType::Identifier)?;
            let mut params = None;

            if self.match_one(TokenType::LeftParen) {
                params = Some(self.parse_effect_params()?);
                self.expect(TokenType::RightParen)?;
            }

            effects.push(Effect {
                name: name_tok.lexeme,
                params,
            });

            if !self.match_one(TokenType::EffectMarker) {
                break;
            }
        }

        Ok(effects)
    }

    // note: parse constraint list
    fn parse_type_constraints(&mut self) -> ParseResult<Vec<TypeConstraint>> {
        let mut constraints = Vec::new();

        while !self.is_at_end() {
            let tok = self.peek().clone();

            match tok.token_type {
                TokenType::Identifier => {
                    let ident = self.advance().lexeme.clone();

                    if self.match_tnv(TokenType::Operator, "=") {
                        let type_ref = self.parse_type()?;
                        constraints.push(TypeConstraint::TypeEq(ident, type_ref.name));
                    } else if self.match_one(TokenType::Colon) {
                        let trait_name = self.expect(TokenType::Identifier)?.lexeme;
                        constraints.push(TypeConstraint::TraitBound(format!(
                            "{}:{}",
                            ident, trait_name
                        )));
                    } else if self.match_tnv(TokenType::Keyword, "subtype") {
                        self.expect_nv(TokenType::Keyword, "of")?;
                        let supertype = self.parse_type()?;
                        constraints.push(TypeConstraint::SubtypeOf(supertype.name));
                    } else {
                        constraints.push(TypeConstraint::TraitBound(ident));
                    }
                }
                TokenType::Keyword if tok.lexeme == "lifetime" => {
                    self.advance();
                    let lifetime = self.expect(TokenType::Identifier)?.lexeme;
                    constraints.push(TypeConstraint::LifetimeBound(lifetime));
                }
                _ => break,
            }

            if !self.match_tnv(TokenType::Operator, "+") {
                break;
            }
        }

        Ok(constraints)
    }

    // note: parse effect param types
    fn parse_effect_params(&mut self) -> ParseResult<Vec<TypeRef>> {
        self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightParen,
            |this| this.parse_type(),
        )
    }

    // note: parse where constraints
    fn parse_where_clauses(&mut self) -> ParseResult<Vec<WhereClause>> {
        let mut clauses = Vec::new();

        while self.peek().token_type != TokenType::LeftBrace
            && self.peek().token_type != TokenType::ShortArrow
        {
            let param_tok = self.expect(TokenType::Identifier)?;
            self.expect(TokenType::Colon)?;
            let constraints = self.parse_type_constraints()?;

            clauses.push(WhereClause {
                param_name: param_tok.lexeme,
                constraints,
            });

            if self.match_one(TokenType::Comma) {
                continue;
            } else {
                break;
            }
        }

        Ok(clauses)
    }

    // note: parse struct declaration
    fn parse_struct(&mut self, attributes: Vec<String>) -> ParseResult<StructDecl> {
        self.expect_nv(TokenType::Keyword, "struct")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();
        self.expect(TokenType::LeftBrace)?;
        let fields = self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightBrace,
            |this| {
                let field_name = this.expect(TokenType::Identifier)?.lexeme;
                this.expect(TokenType::Colon)?;
                let typ = this.parse_type()?;
                Ok((field_name, typ))
            },
        )?;
        self.expect(TokenType::RightBrace)?;
        Ok(StructDecl {
            attributes,
            name,
            fields,
        })
    }

    // note: parse enum with variants
    fn parse_enum(&mut self, attributes: Vec<String>) -> ParseResult<EnumDecl> {
        self.expect_nv(TokenType::Keyword, "enum")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        self.expect(TokenType::LeftBrace)?;

        let mut variants = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let var_tok = self.expect(TokenType::Identifier)?;
            let var_name = var_tok.lexeme;

            let kind = if self.peek().token_type == TokenType::LeftBrace {
                self.advance();
                let fields = self.parse_comma_separated(
                    |tok| tok.token_type == TokenType::RightBrace,
                    |this| {
                        let field_name = this.expect(TokenType::Identifier)?.lexeme;
                        this.expect(TokenType::Colon)?;
                        let typ = this.parse_type()?;
                        Ok(EnumVariantField {
                            name: Some(field_name),
                            typ,
                        })
                    },
                )?;
                self.expect(TokenType::RightBrace)?;
                VariantKind::Struct(fields)
            } else if self.peek().token_type == TokenType::LeftParen {
                self.advance();
                let fields = self.parse_comma_separated(
                    |tok| tok.token_type == TokenType::RightParen,
                    |this| {
                        let is_named = this.peek().token_type == TokenType::Identifier
                            && matches!(
                                this.tokens.get(this.idx + 1),
                                Some(t) if t.token_type == TokenType::Colon
                            );

                        if is_named {
                            let field_name = this.expect(TokenType::Identifier)?.lexeme;
                            this.expect(TokenType::Colon)?;
                            let typ = this.parse_type()?;
                            Ok(EnumVariantField {
                                name: Some(field_name),
                                typ,
                            })
                        } else {
                            let typ = this.parse_type()?;
                            Ok(EnumVariantField { name: None, typ })
                        }
                    },
                )?;
                self.expect(TokenType::RightParen)?;
                VariantKind::Tuple(fields)
            } else {
                VariantKind::Unit
            };

            variants.push(EnumVariant {
                name: var_name,
                kind,
            });

            if self.peek().token_type == TokenType::RightBrace {
                break;
            }
            self.expect(TokenType::Comma)?;
        }

        self.expect(TokenType::RightBrace)?;
        Ok(EnumDecl {
            attributes,
            name,
            generics,
            variants,
        })
    }

    // note: parse trait with methods
    fn parse_trait(&mut self, attributes: Vec<String>) -> ParseResult<TraitDecl> {
        self.expect_nv(TokenType::Keyword, "trait")?;
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        self.expect(TokenType::LeftBrace)?;
        let mut methods = Vec::new();

        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let method_attributes = self.parse_attributes();
            let method_visibility = self.parse_visibility();
            let func = self.parse_function_with(method_attributes, method_visibility)?;
            methods.push(func);
        }

        self.expect(TokenType::RightBrace)?;

        Ok(TraitDecl {
            attributes,
            name,
            methods,
        })
    }

    // note: parse impl block
    fn parse_impl(&mut self, attributes: Vec<String>) -> ParseResult<ImplDecl> {
        self.expect_nv(TokenType::Keyword, "impl")?;

        let first_type = self.parse_type()?;

        let (trait_name, target) = if self.match_tnv(TokenType::Keyword, "for") {
            let trait_name = Some(first_type.name.clone());
            let target = self.parse_type()?;
            (trait_name, target)
        } else {
            (None, first_type)
        };

        self.expect(TokenType::LeftBrace)?;
        let mut methods = Vec::new();

        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let method_attributes = self.parse_attributes();
            let method_visibility = self.parse_visibility();
            let func = self.parse_function_with(method_attributes, method_visibility)?;
            methods.push(func);
        }

        self.expect(TokenType::RightBrace)?;

        Ok(ImplDecl {
            attributes,
            trait_name,
            target,
            methods,
        })
    }

    // note: parse block and track lets
    fn parse_block(&mut self) -> ParseResult<Block> {
        self.expect(TokenType::LeftBrace)?;
        let mut stmts = Vec::new();
        // note: collect let names for auto delete
        let mut let_names = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let s = self.parse_statement()?;

            if let Stmt::Let { pattern, .. } = &s {
                if let PatternKind::Identifier(name) = &pattern.kind {
                    let_names.push(name.clone());
                }
            }
            stmts.push(s);
        }
        self.expect(TokenType::RightBrace)?;

        // note: append delete statements at block end
        for name in let_names {
            stmts.push(Stmt::Delete {
                expr: Expr::Ident(name),
            });
        }
        Ok(Block { stmts })
    }

    // note: parse block or short expr
    fn parse_block_or_expr_body(&mut self) -> ParseResult<Block> {
        if self.peek().token_type == TokenType::LeftBrace {
            return self.parse_block();
        }

        let expr = self.parse_expression(0)?;
        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }

        Ok(Block {
            stmts: vec![Stmt::Expr(expr)],
        })
    }

    // note: avoid struct literal before block
    fn parse_expr_with_struct_literal_guard(
        &mut self,
        allow_struct_literal: bool,
    ) -> ParseResult<Expr> {
        if !allow_struct_literal {
            // note: keep ident { ... } from turning into struct literal
            let tok = self.peek().clone();
            if matches!(
                tok.token_type,
                TokenType::Identifier
                    | TokenType::ModulePath
                    | TokenType::Keyword
                    | TokenType::IGMKeyword
            ) && matches!(
                self.tokens.get(self.idx + 1),
                Some(t) if t.token_type == TokenType::LeftBrace
            ) {
                let ident = self.advance().lexeme.clone();
                let expr = Expr::Ident(ident);
                return self.parse_postfix(expr);
            }
        }

        self.parse_expression(0)
    }

    // note: parse let statement
    fn parse_let_stmt(&mut self) -> ParseResult<Stmt> {
        self.advance();
        let mutable = (!self.is_at_end() && self.match_tnv(TokenType::Operator, "~"))
            .then(|| true)
            .unwrap_or(false);

        let pattern = self.parse_pattern()?;
        let mut typ = None;
        if self.match_one(TokenType::Colon) {
            typ = Some(self.parse_type()?);
        }
        let mut expr = None;
        if self.match_tnv(TokenType::Operator, "=") {
            expr = Some(self.parse_expression(0)?);
        }

        if self.peek().token_type == TokenType::Semicolon {
            self.advance();
        }
        Ok(Stmt::Let {
            pattern,
            mutable,
            typ,
            expr,
        })
    }

    // note: parse try/catch statement
    fn parse_try_catch_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_nv(TokenType::Keyword, "try")?;
        let try_block = self.parse_block()?;
        self.expect_nv(TokenType::Keyword, "catch")?;

        let catch_name = if self.match_one(TokenType::LeftParen) {
            let name = self.expect(TokenType::Identifier)?.lexeme;
            self.expect(TokenType::RightParen)?;
            name
        } else {
            self.expect(TokenType::Identifier)?.lexeme
        };

        let catch_block = self.parse_block()?;

        Ok(Stmt::TryCatch {
            try_block,
            catch_name,
            catch_block,
        })
    }

    // note: parse any statement
    fn parse_statement(&mut self) -> ParseResult<Stmt> {
        if self.peek().lexeme == "in"
            && matches!(
                self.tokens.get(self.idx + 1),
                Some(t) if t.lexeme == "context"
            )
        {
            return self.parse_in_context_stmt();
        }

        if self.peek().lexeme == "on"
            && matches!(
                self.tokens.get(self.idx + 1),
                Some(t) if t.token_type == TokenType::Keyword && t.lexeme == "sequence"
            )
        {
            return self.parse_on_sequence_stmt();
        }

        match self.peek().lexeme.as_str() {
            "let" => self.parse_let_stmt(),
            "try" => self.parse_try_catch_stmt(),
            "continue" => {
                self.expect_nv(TokenType::Keyword, "continue")?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Continue)
            }
            "break" => {
                self.expect_nv(TokenType::Keyword, "break")?;
                let expr = if self.peek().token_type != TokenType::Semicolon
                    && self.peek().token_type != TokenType::RightBrace
                {
                    Some(self.parse_expression(0)?)
                } else {
                    None
                };
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Break)
            }
            "panic" => {
                self.expect_nv(TokenType::Keyword, "panic")?;
                self.expect_nv(TokenType::Keyword, "unless")?;
                let condition = self.parse_expression(0)?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::PanicUnless { condition })
            }
            "return" => {
                self.advance();
                let expr = if self.peek().token_type != TokenType::Semicolon {
                    Some(self.parse_expression(0)?)
                } else {
                    None
                };
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Return(expr))
            }
            "while" => {
                self.advance();
                let cond = self.parse_expression(0)?;
                let body = self.parse_block()?;
                Ok(Stmt::While { cond, body })
            }
            "for" => {
                self.advance();
                let pat_tok = self.expect(TokenType::Identifier)?;
                let pat = pat_tok.lexeme.clone();
                self.expect_nv(TokenType::Keyword, "in")?;

                let iter = self.parse_expr_with_struct_literal_guard(false)?;

                let body = if self.match_one(TokenType::ShortArrow) {
                    self.parse_block_or_expr_body()?
                } else {
                    self.parse_block()?
                };
                Ok(Stmt::For { pat, iter, body })
            }
            "loop" => {
                self.advance();
                let body = self.parse_block()?;
                Ok(Stmt::Loop { body })
            }
            "match" => {
                self.expect_nv(TokenType::Keyword, "match")?;

                if self.peek().token_type == TokenType::LeftBrace {
                    self.advance();
                    let mut arms = Vec::new();
                    while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
                        let mut condition = self.parse_expression(1)?;
                        while self.peek().token_type == TokenType::FatArrow {
                            self.advance();
                            let rhs = self.parse_expression(1)?;
                            condition = Expr::Binary {
                                left: Box::new(condition),
                                op: "->".into(),
                                right: Box::new(rhs),
                            };
                        }
                        self.expect(TokenType::ShortArrow)?;
                        let body = self.parse_block_or_expr_body()?;
                        arms.push(MatchCondArm { condition, body });
                    }
                    self.expect(TokenType::RightBrace)?;
                    Ok(Stmt::MatchCond { arms })
                } else {
                    let expr = self.parse_expr_with_struct_literal_guard(false)?;
                    self.expect(TokenType::LeftBrace)?;

                    let mut arms = Vec::new();
                    while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
                        let pattern = self.parse_pattern()?;
                        let guard = if self.match_tnv(TokenType::Keyword, "if") {
                            Some(self.parse_expression(0)?)
                        } else {
                            None
                        };
                        self.expect(TokenType::ShortArrow)?;
                        let body = self.parse_expression(0)?;

                        arms.push(MatchArm {
                            pattern,
                            guard,
                            body,
                        });
                    }

                    self.expect(TokenType::RightBrace)?;
                    Ok(Stmt::Match { expr, arms })
                }
            }
            "if" => {
                let next_idx = self.idx + 1;
                self.advance();
                let cond = self.parse_expression(0)?;
                let then_block = self.parse_block()?;
                let else_block = if self.peek().lexeme == "else" {
                    self.advance();
                    Some(self.parse_block()?)
                } else {
                    None
                };
                Ok(Stmt::If {
                    cond,
                    then_block,
                    else_block,
                })
            }
            "watch" => {
                self.expect_nv(TokenType::Keyword, "watch")?;

                let mut variables = Vec::new();
                loop {
                    variables.push(self.parse_expr_with_struct_literal_guard(false)?);
                    if self.match_one(TokenType::Comma) {
                        continue;
                    } else {
                        break;
                    }
                }

                self.expect(TokenType::LeftBrace)?;

                let mut clauses = Vec::new();
                while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
                    self.expect_nv(TokenType::Keyword, "when")?;
                    let condition = self.parse_expression(0)?;
                    self.expect(TokenType::ShortArrow)?;
                    let body = self.parse_block_or_expr_body()?;

                    clauses.push(WatchClause { condition, body });
                }

                self.expect(TokenType::RightBrace)?;
                Ok(Stmt::Watch { variables, clauses })
            }
            "converge" => {
                self.expect_nv(TokenType::Keyword, "converge")?;
                self.expect_nv(TokenType::Keyword, "with")?;

                let history = self.match_tnv(TokenType::Keyword, "history");
                let variable = self.expect(TokenType::Identifier)?.lexeme;

                let body = self.parse_block()?;
                self.expect_nv(TokenType::Keyword, "until")?;
                let until = self.parse_expression(0)?;

                Ok(Stmt::Converge {
                    history,
                    variable,
                    body,
                    until,
                })
            }
            "within" => {
                self.expect_nv(TokenType::Keyword, "within")?;
                let time = self.parse_expression(0)?;

                let condition = if self.match_tnv(TokenType::Keyword, "if") {
                    Some(self.parse_expression(0)?)
                } else {
                    None
                };

                self.expect(TokenType::ShortArrow)?;
                let body = self.parse_block_or_expr_body()?;

                Ok(Stmt::Within {
                    time,
                    condition,
                    body,
                })
            }
            "atomically" => {
                self.expect(TokenType::Keyword)?;
                let body = self.parse_block()?;
                Ok(Stmt::Atomically { body })
            }
            "trap" => {
                self.expect_nv(TokenType::Keyword, "trap")?;
                let error_condition = self.parse_expression(0)?;
                self.expect(TokenType::ShortArrow)?;
                let body = self.parse_block_or_expr_body()?;
                Ok(Stmt::Trap {
                    error_condition,
                    body,
                })
            }
            "guard" => {
                self.expect_nv(TokenType::Keyword, "guard")?;
                let condition = self.parse_expression(0)?;
                self.expect(TokenType::ShortArrow)?;
                let then_block = self.parse_block_or_expr_body()?;

                let else_block = if self.match_tnv(TokenType::Keyword, "else") {
                    self.expect(TokenType::ShortArrow)?;
                    Some(self.parse_block_or_expr_body()?)
                } else {
                    None
                };

                Ok(Stmt::Guard {
                    condition,
                    then_block,
                    else_block,
                })
            }
            "poll" => {
                self.expect_nv(TokenType::Keyword, "poll")?;
                let signal = self.parse_expression(0)?;
                self.expect_nv(TokenType::Keyword, "with")?;
                if self.peek().lexeme == "interval" {
                    self.advance();
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: "interval".into(),
                        found: self.peek().clone(),
                        idx: self.idx,
                    });
                }
                self.expect_nv(TokenType::Operator, "=")?;
                let interval = self.parse_expression(0)?;
                self.expect(TokenType::ShortArrow)?;
                let body = self.parse_block_or_expr_body()?;

                Ok(Stmt::Poll {
                    signal,
                    interval,
                    body,
                })
            }
            "delete" => {
                self.expect(TokenType::Keyword)?;
                let expr = self.parse_expression(0)?;

                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }

                Ok(Stmt::Delete { expr })
            }
            "joins" => {
                self.expect(TokenType::Keyword)?;
                self.expect(TokenType::LeftParen)?;
                let mut decls = Vec::new();
                while self.peek().token_type != TokenType::RightParen && !self.is_at_end() {
                    let decl = self.parse_let_stmt()?;
                    decls.push(decl);
                    if !self.match_one(TokenType::Semicolon) {
                        continue;
                    }
                }
                self.expect(TokenType::RightParen)?;
                let body = self.parse_block()?;
                Ok(Stmt::Joins { decls, body })
            }
            _ => {
                let expr = self.parse_expression(0)?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Expr(expr))
            }
        }
    }

    // note: parse in context block
    fn parse_in_context_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_nv(TokenType::Keyword, "in")?;
        if self.peek().lexeme == "context" {
            self.advance();
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "context".into(),
                found: self.peek().clone(),
                idx: self.idx,
            });
        }
        let target = self.parse_expr_with_struct_literal_guard(false)?;
        self.expect(TokenType::LeftBrace)?;

        let mut arms = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let value = self.parse_expression(0)?;
            self.expect(TokenType::ShortArrow)?;
            let body = self.parse_block_or_expr_body()?;
            arms.push(ContextArm { value, body });
        }
        self.expect(TokenType::RightBrace)?;

        Ok(Stmt::InContext { target, arms })
    }

    // note: parse on sequence block
    fn parse_on_sequence_stmt(&mut self) -> ParseResult<Stmt> {
        self.expect_nv(TokenType::Keyword, "on")?;
        self.expect_nv(TokenType::Keyword, "sequence")?;

        let target = self.parse_expression(1)?;
        self.expect_nv(TokenType::Operator, "=")?;
        let sequence = self.parse_expression(0)?;

        self.expect(TokenType::ShortArrow)?;
        let body = self.parse_block_or_expr_body()?;

        Ok(Stmt::OnSequence {
            target,
            sequence,
            body,
        })
    }

    // note: Pratt expression parse
    fn parse_expression(&mut self, min_prec: u8) -> ParseResult<Expr> {
        let mut left = self.parse_unary_or_primary()?;

        loop {
            let op_tok = self.peek().clone();
            let (prec, right_assoc) = match op_precedence(&op_tok) {
                Some((p, ra)) => (p, ra),
                None => break,
            };

            if prec < min_prec {
                break;
            }

            let op_lex = self.advance().lexeme.clone();

            if op_lex == "?" {
                left = Expr::Try(Box::new(left));
                continue;
            }

            let next_min = if right_assoc { prec } else { prec + 1 };
            let right = self.parse_expression(next_min)?;

            if op_lex == "|>" {
                left = Expr::Pipeline {
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else {
                left = Expr::Binary {
                    left: Box::new(left),
                    op: op_lex,
                    right: Box::new(right),
                };
            }
        }

        Ok(left)
    }

    // note: unary then primary+postfix
    fn parse_unary_or_primary(&mut self) -> ParseResult<Expr> {
        let tok = self.peek().clone();

        if tok.token_type == TokenType::Keyword {
            if tok.lexeme == "await" {
                self.advance();
                let rhs = self.parse_expression(13)?;
                return Ok(Expr::Await(Box::new(rhs)));
            }

            if tok.lexeme == "spawn" {
                self.advance();
                let rhs = self.parse_expression(13)?;
                return Ok(Expr::Spawn(Box::new(rhs)));
            }
        }

        if matches!(tok.token_type, TokenType::Operator) {
            if tok.lexeme == "-" || tok.lexeme == "+" || tok.lexeme == "!" || tok.lexeme == "not" {
                let op = self.advance().lexeme.clone();
                let rhs = self.parse_expression(100)?;
                return Ok(Expr::Unary {
                    op,
                    rhs: Box::new(rhs),
                });
            }
        }

        let primary = self.parse_primary()?;
        self.parse_postfix(primary)
    }

    // note: parse literals, ids, groups
    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let tok = self.peek().clone();
        match tok.token_type {
            TokenType::LambdaArrow => {
                self.advance();
                self.parse_lambda()
            }
            TokenType::IntLiteral => {
                self.advance();
                Ok(Expr::Literal(Literal::Int(tok.lexeme)))
            }
            TokenType::FloatLiteral => {
                self.advance();
                Ok(Expr::Literal(Literal::Float(tok.lexeme)))
            }
            TokenType::UnitLiteral => {
                self.advance();
                let lex = tok.lexeme;
                let mut split_idx = 0;
                for (i, ch) in lex.char_indices() {
                    if !(ch.is_ascii_digit() || ch == '.') {
                        split_idx = i;
                        break;
                    }
                }
                if split_idx == 0 {
                    return Ok(Expr::Literal(Literal::Unit {
                        v: lex,
                        u: "".into(),
                    }));
                }
                let (v, u) = lex.split_at(split_idx);
                Ok(Expr::Literal(Literal::Unit {
                    v: v.to_string(),
                    u: u.to_string(),
                }))
            }
            TokenType::StringLiteral
            | TokenType::InterpolatedStringLiteral
            | TokenType::MultilineStringLiteral => {
                self.advance();
                Ok(Expr::Literal(Literal::String(tok.lexeme)))
            }
            TokenType::BoolLiteral => {
                self.advance();
                let b = tok.lexeme == "true";
                Ok(Expr::Literal(Literal::Bool(b)))
            }
            TokenType::Identifier
            | TokenType::ModulePath
            | TokenType::Keyword
            | TokenType::IGMKeyword => {
                let ident = self.advance().lexeme.clone();
                if self.peek().token_type == TokenType::LeftBrace {
                    self.advance();
                    let fields = self.parse_struct_fields()?;
                    Ok(Expr::Literal(Literal::Struct {
                        name: ident,
                        base: None,
                        fields,
                    }))
                } else if ident == "v" && self.peek().token_type == TokenType::LeftBracket {
                    self.advance();
                    let elements = self.parse_comma_separated(
                        |tok| tok.token_type == TokenType::RightBracket,
                        |this| this.parse_expression(0),
                    )?;
                    self.expect(TokenType::RightBracket)?;
                    Ok(Expr::Literal(Literal::Vector(elements)))
                } else {
                    Ok(Expr::Ident(ident))
                }
            }
            TokenType::LeftParen => {
                self.advance();
                let mut exprs = Vec::new();
                let mut trailing_comma = false;
                while !self.is_at_end() && self.peek().token_type != TokenType::RightParen {
                    let expr = self.parse_expression(0)?;
                    exprs.push(expr);
                    if self.match_one(TokenType::Comma) {
                        trailing_comma = true;
                        continue;
                    } else {
                        trailing_comma = false;
                        break;
                    }
                }
                self.expect(TokenType::RightParen)?;
                if exprs.len() == 1 && !trailing_comma {
                    Ok(Expr::Grouping(Box::new(exprs.remove(0))))
                } else {
                    Ok(Expr::Literal(Literal::Tuple(exprs)))
                }
            }
            TokenType::LeftBracket => {
                self.advance();
                if self.peek().token_type == TokenType::RightBracket {
                    self.advance();
                    return Ok(Expr::Literal(Literal::Array(Vec::new())));
                }

                if let Some(pipe_idx) = self.find_list_comp_pipe(self.idx) {
                    if pipe_idx == self.idx {
                        return Err(self.error_here("expected expression before list comprehension"));
                    }

                    let first_expr = self.parse_expression_slice(pipe_idx)?;
                    self.expect(TokenType::Pipe)?;

                    // note: list comp: [expr | pat <- iter, guard]
                    let pattern = self.parse_pattern()?;
                    self.expect_nv(TokenType::Operator, "<-")?;
                    let iter = self.parse_expression(0)?;

                    let guard = if self.match_one(TokenType::Comma) {
                        Some(Box::new(self.parse_expression(0)?))
                    } else {
                        None
                    };

                    self.expect(TokenType::RightBracket)?;
                    Ok(Expr::ListComp {
                        expr: Box::new(first_expr),
                        pattern,
                        iter: Box::new(iter),
                        guard,
                    })
                } else {
                    let first_expr = self.parse_expression(0)?;
                    let mut elements = vec![first_expr];
                    while self.match_one(TokenType::Comma) {
                        if self.peek().token_type == TokenType::RightBracket {
                            break;
                        }
                        let elem = self.parse_expression(0)?;
                        elements.push(elem);
                    }
                    self.expect(TokenType::RightBracket)?;
                    Ok(Expr::Literal(Literal::Array(elements)))
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "expression".into(),
                found: tok,
                idx: self.idx,
            }),
        }
    }

    // note: look for list comp pipe at top-level in current bracket
    fn find_list_comp_pipe(&self, start_idx: usize) -> Option<usize> {
        let mut depth_paren: i32 = 0;
        let mut depth_brace: i32 = 0;
        let mut depth_bracket: i32 = 0;
        let mut i = start_idx;

        while i < self.tokens.len() {
            let tok = &self.tokens[i];
            let at_top = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;

            if at_top {
                match tok.token_type {
                    TokenType::RightBracket | TokenType::Comma => break,
                    TokenType::Pipe => {
                        if self.has_list_comp_arrow(i + 1) {
                            return Some(i);
                        }
                    }
                    _ => {}
                }
            }

            match tok.token_type {
                TokenType::LeftParen => depth_paren += 1,
                TokenType::RightParen => {
                    if depth_paren > 0 {
                        depth_paren -= 1;
                    }
                }
                TokenType::LeftBrace => depth_brace += 1,
                TokenType::RightBrace => {
                    if depth_brace > 0 {
                        depth_brace -= 1;
                    }
                }
                TokenType::LeftBracket => depth_bracket += 1,
                TokenType::RightBracket => {
                    if depth_bracket > 0 {
                        depth_bracket -= 1;
                    }
                }
                _ => {}
            }

            i += 1;
        }

        None
    }

    // note: check for "<-" at top-level after the pipe
    fn has_list_comp_arrow(&self, start_idx: usize) -> bool {
        let mut depth_paren: i32 = 0;
        let mut depth_brace: i32 = 0;
        let mut depth_bracket: i32 = 0;
        let mut saw_token = false;
        let mut i = start_idx;

        while i < self.tokens.len() {
            let tok = &self.tokens[i];
            let at_top = depth_paren == 0 && depth_brace == 0 && depth_bracket == 0;

            if at_top {
                match tok.token_type {
                    TokenType::RightBracket | TokenType::Comma => return false,
                    TokenType::Operator if tok.lexeme == "<-" => return saw_token,
                    TokenType::EOF => return false,
                    _ => {}
                }
            }

            saw_token = true;

            match tok.token_type {
                TokenType::LeftParen => depth_paren += 1,
                TokenType::RightParen => {
                    if depth_paren > 0 {
                        depth_paren -= 1;
                    }
                }
                TokenType::LeftBrace => depth_brace += 1,
                TokenType::RightBrace => {
                    if depth_brace > 0 {
                        depth_brace -= 1;
                    }
                }
                TokenType::LeftBracket => depth_bracket += 1,
                TokenType::RightBracket => {
                    if depth_bracket > 0 {
                        depth_bracket -= 1;
                    }
                }
                _ => {}
            }

            i += 1;
        }

        false
    }

    // note: parse expression from current idx up to end_idx (exclusive)
    fn parse_expression_slice(&mut self, end_idx: usize) -> ParseResult<Expr> {
        let sub_tokens = self.tokens[self.idx..end_idx].to_vec();
        let mut sub_parser = Parser::new(sub_tokens);
        let expr = sub_parser.parse_expression(0)?;

        if !sub_parser.is_at_end() {
            let tok = sub_parser.peek().clone();
            return Err(ParseError::UnexpectedToken {
                expected: "end of list comprehension expression".into(),
                found: tok,
                idx: self.idx + sub_parser.idx,
            });
        }

        self.idx = end_idx;
        Ok(expr)
    }

    // note: parse calls, fields, index, update
    fn parse_postfix(&mut self, left: Expr) -> ParseResult<Expr> {
        let mut expr = left;
        loop {
            match self.peek().token_type {
                TokenType::Dot => {
                    self.advance();
                    let field_or_method = self.expect(TokenType::Identifier)?.lexeme;
                    let mut generics = Vec::new();
                    if self.peek().token_type == TokenType::Operator && self.peek().lexeme == "<" {
                        self.advance();
                        generics = self.parse_generic_args()?;
                    }
                    if self.peek().token_type == TokenType::LeftParen {
                        self.advance();
                        let args = self.parse_arguments()?;
                        self.expect(TokenType::RightParen)?;
                        expr = Expr::MethodCall {
                            object: Box::new(expr),
                            method: field_or_method,
                            generics,
                            args,
                        };
                    } else {
                        if !generics.is_empty() {
                            return Err(self.error_here(
                                "Unexpected generics without method call",
                            ));
                        }
                        expr = Expr::FieldAccess {
                            object: Box::new(expr),
                            field: field_or_method,
                        };
                    }
                }
                TokenType::LeftBracket => {
                    self.advance();
                    let index_expr = self.parse_expression(0)?;
                    self.expect(TokenType::RightBracket)?;
                    expr = Expr::Index {
                        array: Box::new(expr),
                        index: Box::new(index_expr),
                    };
                }
                TokenType::Operator if self.peek().lexeme == "<" => {
                    // note: look ahead to confirm <...>( ) call
                    let mut depth: i32 = 0;
                    let mut found_generic_call = false;
                    for la in 0..64usize {
                        if let Some(t) = self.tokens.get(self.idx + la) {
                            if t.token_type == TokenType::Operator && t.lexeme == "<" {
                                depth += 1;
                            } else if t.token_type == TokenType::Operator && t.lexeme == ">" {
                                depth -= 1;
                                if depth == 0 {
                                    if matches!(
                                        self.tokens.get(self.idx + la + 1),
                                        Some(n) if n.token_type == TokenType::LeftParen
                                    ) {
                                        found_generic_call = true;
                                    }
                                    break;
                                }
                            } else if t.token_type == TokenType::Operator && t.lexeme == ">>" {
                                depth -= 2;
                                if depth <= 0 {
                                    if matches!(
                                        self.tokens.get(self.idx + la + 1),
                                        Some(n) if n.token_type == TokenType::LeftParen
                                    ) {
                                        found_generic_call = true;
                                    }
                                    break;
                                }
                            }
                        } else {
                            break;
                        }
                    }

                    if !found_generic_call {
                        break;
                    }

                    self.advance();
                    let generics = self.parse_generic_args()?;
                    self.expect(TokenType::LeftParen)?;
                    let args = self.parse_arguments()?;
                    self.expect(TokenType::RightParen)?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        generics,
                        args,
                    };
                }
                TokenType::LeftParen => {
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(TokenType::RightParen)?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        generics: Vec::new(),
                        args,
                    };
                }
                TokenType::Keyword if self.peek().lexeme == "with" => {
                    if !matches!(
                        self.tokens.get(self.idx + 1),
                        Some(t) if t.token_type == TokenType::LeftBrace
                    ) {
                        break;
                    }

                    // note: record update: base with { fields }
                    self.advance();
                    self.expect(TokenType::LeftBrace)?;
                    let fields = self.parse_struct_fields()?;

                    expr = Expr::Literal(Literal::Struct {
                        name: "update".to_string(),
                        base: Some(Box::new(expr)),
                        fields,
                    });
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    // note: parse call argument list
    fn parse_arguments(&mut self) -> ParseResult<Vec<Expr>> {
        self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightParen,
            |this| this.parse_expression(0),
        )
    }

    // note: parse generic args until >
    fn parse_generic_args(&mut self) -> ParseResult<Vec<TypeRef>> {
        if self.peek().token_type == TokenType::Operator && self.peek().lexeme == ">" {
            self.expect_gt()?;
            return Ok(Vec::new());
        }
        let generics = self.parse_comma_separated(
            |tok| {
                tok.token_type == TokenType::Operator && (tok.lexeme == ">" || tok.lexeme == ">>")
            },
            |this| this.parse_type(),
        )?;
        self.expect_gt()?;
        Ok(generics)
    }

    // note: parse {field: expr} list
    fn parse_struct_fields(&mut self) -> ParseResult<Vec<(String, Expr)>> {
        let fields = self.parse_comma_separated(
            |tok| tok.token_type == TokenType::RightBrace,
            |this| {
                let field_name = this.expect(TokenType::Identifier)?.lexeme;
                this.expect(TokenType::Colon)?;
                let field_expr = this.parse_expression(0)?;
                Ok((field_name, field_expr))
            },
        )?;
        self.expect(TokenType::RightBrace)?;
        Ok(fields)
    }

    // note: parse type with generics and nullable
    fn parse_type(&mut self) -> ParseResult<TypeRef> {
        let mut name = String::new();
        let mut generics = Vec::new();
        let mut nullable = false;
        let mut pointer_type = None;

        let mut tok = self.peek().clone();
        if tok.token_type == TokenType::Operator {
            pointer_type = match tok.lexeme.as_str() {
                "~" => Some(PointerType::RawPointer),
                "@" => Some(PointerType::ManagedPointer),
                "&" => Some(PointerType::WeakPointer),
                "+" => Some(PointerType::SharedPointer),
                _ => None,
            };
            self.advance();
        }

        tok = self.peek().clone();
        match tok.token_type {
            TokenType::Identifier | TokenType::Keyword | TokenType::ModulePath => {
                name = tok.lexeme.clone();
                self.advance();
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "Type identifier".into(),
                    found: tok,
                    idx: self.idx,
                })
            }
        }

        if self.match_tnv(TokenType::Operator, "<") {
            generics = self.parse_generic_args()?;
        }

        if self.match_tnv(TokenType::Operator, "?") {
            nullable = true;
        }

        Ok(TypeRef {
            name,
            generics,
            nullable,
            pointer_type,
        })
    }

    // note: parse list of patterns
    fn parse_pattern_list(&mut self, end: TokenType) -> ParseResult<(Vec<Pattern>, Vec<String>)> {
        let mut patterns = Vec::new();
        let mut bindings = Vec::new();
        while !self.is_at_end() && self.peek().token_type != end {
            let sub_pat = self.parse_pattern()?;
            bindings.extend(sub_pat.bindings.clone());
            patterns.push(sub_pat);
            if self.match_one(TokenType::Comma) {
                if self.peek().token_type == end {
                    break;
                }
                continue;
            } else {
                break;
            }
        }
        self.expect(end)?;
        Ok((patterns, bindings))
    }

    // note: parse struct pattern fields
    fn parse_struct_pattern_fields(
        &mut self,
    ) -> ParseResult<(Vec<(String, Pattern)>, Vec<String>)> {
        let mut fields = Vec::new();
        let mut bindings = Vec::new();
        while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
            let field_name = self.expect(TokenType::Identifier)?.lexeme;
            if self.match_one(TokenType::Colon) {
                let field_pat = self.parse_pattern()?;
                bindings.extend(field_pat.bindings.clone());
                fields.push((field_name, field_pat));
            } else {
                let field_pat = Pattern {
                    kind: PatternKind::Identifier(field_name.clone()),
                    bindings: vec![field_name.clone()],
                };
                bindings.push(field_name.clone());
                fields.push((field_name, field_pat));
            }

            if self.match_one(TokenType::Comma) {
                if self.peek().token_type == TokenType::RightBrace {
                    break;
                }
                continue;
            } else {
                break;
            }
        }
        self.expect(TokenType::RightBrace)?;
        Ok((fields, bindings))
    }

    // note: parse pattern and bindings
    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let tok = self.peek().clone();
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
            TokenType::Identifier => {
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
    fn parse_lambda(&mut self) -> ParseResult<Expr> {
        let mut params = Vec::new();

        let param_name = self.expect(TokenType::Identifier)?.lexeme;
        params.push(Param {
            pattern: Pattern {
                kind: PatternKind::Identifier(param_name.clone()),
                bindings: vec![param_name],
            },
            typ: None,
            default: None,
        });

        while self.match_one(TokenType::Comma) {
            let param_name = self.expect(TokenType::Identifier)?.lexeme;
            params.push(Param {
                pattern: Pattern {
                    kind: PatternKind::Identifier(param_name.clone()),
                    bindings: vec![param_name],
                },
                typ: None,
                default: None,
            });
        }

        self.expect(TokenType::ShortArrow)?;

        let body = self.parse_expression(0)?;

        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
        })
    }
}

// note: operator precedence table
pub fn op_precedence(tok: &Token) -> Option<(u8, bool)> {
    match tok.lexeme.as_str() {
        "||" | "or" => Some((1, false)),
        "&&" | "and" => Some((2, false)),
        "==" | "!=" | "<" | "<=" | ">" | ">=" => Some((3, false)),
        "|>" => Some((11, false)),
        "|" => Some((4, false)),
        "^" => Some((5, false)),
        "&" => Some((6, false)),
        "<<" | ">>" => Some((7, false)),
        "+" | "-" => Some((8, false)),
        "*" | "/" | "%" => Some((9, false)),
        "^." => Some((10, false)),
        "::" => Some((12, false)),
        "?" => Some((13, false)),
        "=" | ":=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" => {
            Some((0, true))
        }
        _ => None,
    }
}
