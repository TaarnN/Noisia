#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(dead_code)]

// parser.rs
use crate::ptypes::*;
use crate::tokenizer::{Token, TokenType};
use std::fmt;

//
// fmt::Display
//

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::ModuleDecl(inner) => write!(f, "{:#?}", inner),
            Item::Import(inner) => write!(f, "{:#?}", inner),
            Item::Function(inner) => write!(f, "{:#?}", inner),
            Item::Struct(inner) => write!(f, "{:#?}", inner),
            Item::Enum(inner) => write!(f, "{:#?}", inner),
            Item::Trait(inner) => write!(f, "{:#?}", inner), // MISSING!!!
            Item::Impl(inner) => write!(f, "{:#?}", inner),  // MISSING!!!
            Item::Class(inner) => write!(f, "{:#?}", inner),
        }
    }
}

//
// Parser
//
pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, idx: 0 }
    }

    // helpers
    fn peek(&self) -> &Token {
        self.tokens.get(self.idx).unwrap_or_else(|| {
            // create a synthetic EOF Token (never owned by tokenizer, but handy)
            static EOF_TOKEN: Token = Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                line: 0,
                column: 0,
            };
            &EOF_TOKEN
        })
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().token_type, TokenType::EOF)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.idx += 1;
        }
        &self.tokens[self.idx.saturating_sub(1)]
    }

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

    fn match_one(&mut self, t: TokenType) -> bool {
        if self.peek().token_type == t {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_tnv(&mut self, t: TokenType, ch: &str) -> bool {
        let token = self.peek();
        if (token.token_type == t) && (token.lexeme == ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    // Match a single '>' operator, splitting a '>>' token into two '>' tokens when needed.
    // This lets us parse nested generic types without breaking shift operators in expressions.
    fn match_gt(&mut self) -> bool {
        let tok = self.peek().clone();
        if tok.token_type == TokenType::Operator && tok.lexeme == ">" {
            self.advance();
            return true;
        }

        if tok.token_type == TokenType::Operator && tok.lexeme == ">>" {
            // Split ">>" into two ">" tokens at the current index.
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

    // top-level parse entry
    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut items = Vec::new();
        while !self.is_at_end() {
            let item = self.parse_item()?;
            items.push(item);
        }
        Ok(Program { items })
    }

    // In parser.rs, replace the entire `parse_item` method with this updated version:
    fn parse_item(&mut self) -> ParseResult<Item> {
        // Parse leading attributes (common for functions, classes, etc.)
        let mut attributes = Vec::new();
        while self.peek().token_type == TokenType::Attribute
            || self.peek().token_type == TokenType::ParameterizedAttribute
        {
            attributes.push(self.advance().lexeme.clone());
        }

        // Now inspect the next token to decide what kind of item this is
        if self.peek().token_type != TokenType::Keyword {
            return Err(ParseError::Generic(format!(
                "Expected keyword after attributes, found {}",
                self.peek()
            )));
        }

        let kw = self.peek().lexeme.clone();
        match kw.as_str() {
            "module" => {
                self.advance();
                let path = self.parse_module_path_string()?;
                Ok(Item::ModuleDecl(ModuleDecl { path }))
            }
            "import" => {
                self.advance();
                let (path, symbols) = self.parse_import()?;
                Ok(Item::Import(ImportDecl { path, symbols }))
            }
            "fn" | "async" => {
                // Function (leading attributes already captured)
                let mut func = self.parse_function()?;
                func.attributes = attributes;
                Ok(Item::Function(func))
            }
            "struct" => {
                let s = self.parse_struct()?;
                Ok(Item::Struct(s))
            }
            "enum" => {
                let e = self.parse_enum()?;
                Ok(Item::Enum(e))
            }
            "trait" => {
                let t = self.parse_trait()?;
                Ok(Item::Trait(t))
            }
            "impl" => {
                let i = self.parse_impl()?;
                Ok(Item::Impl(i))
            }
            "class" => {
                // Class (leading attributes ignored for now; ClassDecl has no field for them)
                let c = self.parse_class()?;
                Ok(Item::Class(c))
            }
            _ => Err(ParseError::Generic(format!(
                "Unsupported top-level keyword: {}",
                kw
            ))),
        }
    }

    fn parse_class(&mut self) -> ParseResult<ClassDecl> {
        self.expect_nv(TokenType::Keyword, "class")?;

        let name_tok = self.expect_ident_like()?;
        let name = name_tok.lexeme;

        while self.peek().token_type == TokenType::Keyword
            && (self.peek().lexeme == "extends"
                || self.peek().lexeme == "with"
                || self.peek().lexeme == "implements")
        {
            self.advance();

            let _ = self.parse_type()?;
            while self.match_one(TokenType::Comma) {
                let _ = self.parse_type()?;
            }
        }

        self.expect(TokenType::LeftBrace)?;

        let mut fields = Vec::new();
        let mut ctors = Vec::new();
        let mut methods = Vec::new();

        while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
            let (attributes, vis) = self.parse_member_attributes_and_visibility()?;

            if self.peek().token_type == TokenType::Keyword && self.peek().lexeme == "init" {
                let ctor = self.parse_constructor(attributes, vis)?;
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
                let method = self.parse_method(attributes, vis)?;
                methods.push(method);
                if self.peek().token_type == TokenType::Comma
                    || self.peek().token_type == TokenType::Semicolon
                {
                    self.advance();
                }
                continue;
            }

            let is_field = (self.peek().token_type == TokenType::Keyword
                && self.peek().lexeme == "mutable"
                && self
                    .tokens
                    .get(self.idx + 1)
                    .map_or(false, |t| t.token_type == TokenType::Identifier)
                && self
                    .tokens
                    .get(self.idx + 2)
                    .map_or(false, |t| t.token_type == TokenType::Colon))
                || (self.peek().token_type == TokenType::Identifier
                    && matches!(
                        self.tokens.get(self.idx + 1),
                        Some(t) if t.token_type == TokenType::Colon
                    ))
                || (self.peek().token_type == TokenType::Identifier
                    && matches!(
                        self.tokens.get(self.idx + 1),
                        Some(t) if t.token_type == TokenType::Keyword && t.lexeme == "mutable"
                    )
                    && self
                        .tokens
                        .get(self.idx + 2)
                        .map_or(false, |t| t.token_type == TokenType::Identifier)
                    && self
                        .tokens
                        .get(self.idx + 3)
                        .map_or(false, |t| t.token_type == TokenType::Colon));

            if is_field {
                let field = self.parse_class_field(attributes, vis)?;
                fields.push(field);
                continue;
            }

            return Err(ParseError::Generic(format!(
                "Unsupported member in class body: {}",
                self.peek()
            )));
        }

        self.expect(TokenType::RightBrace)?;
        Ok(ClassDecl {
            name,
            fields,
            ctors,
            methods,
        })
    }

    fn parse_member_attributes_and_visibility(&mut self) -> ParseResult<(Vec<String>, Visibility)> {
        let mut attributes = Vec::new();
        while self.peek().token_type == TokenType::Attribute
            || self.peek().token_type == TokenType::ParameterizedAttribute
        {
            attributes.push(self.advance().lexeme.clone());
        }

        let vis = if self.match_tnv(TokenType::Keyword, "public") {
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
        };

        Ok((attributes, vis))
    }

    fn parse_class_field(
        &mut self,
        attributes: Vec<String>,
        vis: Visibility,
    ) -> ParseResult<ClassFieldDecl> {
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

        Ok(ClassFieldDecl {
            attributes,
            vis,
            mutable,
            name: name_tok.lexeme,
            typ,
            value,
        })
    }

    fn parse_constructor(
        &mut self,
        _attributes: Vec<String>,
        _vis: Visibility,
    ) -> ParseResult<ConstructorDecl> {
        self.expect_nv(TokenType::Keyword, "init")?;

        let ctor_name = if self.match_one(TokenType::Dot) {
            Some(self.expect(TokenType::Identifier)?.lexeme)
        } else {
            None
        };

        self.expect(TokenType::LeftParen)?;
        let mut params = Vec::new();
        while self.peek().token_type != TokenType::RightParen {
            let pattern = self.parse_pattern()?;
            let mut typ = None;
            if self.match_one(TokenType::Colon) {
                typ = Some(self.parse_type()?);
            }
            let mut default = None;
            if self.match_tnv(TokenType::Operator, "=") {
                default = Some(self.parse_expression(0)?);
            }
            params.push(Param {
                pattern,
                typ,
                default,
            });
            if self.match_one(TokenType::Comma) {
                continue;
            } else {
                break;
            }
        }
        self.expect(TokenType::RightParen)?;

        if self.peek().token_type != TokenType::LeftBrace {
            return Err(ParseError::Generic(
                "Constructors must have a block body".into(),
            ));
        }
        let body = self.parse_block()?;

        Ok(ConstructorDecl {
            name: ctor_name,
            params,
            body,
        })
    }

    fn parse_method(
        &mut self,
        attributes: Vec<String>,
        visibility: Visibility,
    ) -> ParseResult<FunctionDecl> {
        let mut f = self.parse_function()?;
        f.attributes = attributes;
        f.visibility = visibility;
        Ok(f)
    }

    // parse module path like App::Hello or Identifier or ModulePath token
    fn parse_module_path_string(&mut self) -> ParseResult<String> {
        // tokenizer already produces ModulePath token; accept it or build from ident+::...
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

    fn parse_import(&mut self) -> ParseResult<(String, Option<Vec<String>>)> {
        // simple import: import Core::{List, Option}
        let path = self.parse_module_path_string()?;
        if self.match_one(TokenType::Colon) || self.peek().token_type == TokenType::DoubleColon {
            // if tokenizer uses DoubleColon token type, handle above
            // but we already consumed path; check for `{`
        }
        // optional symbols block
        if self.match_one(TokenType::LeftBrace) {
            let mut syms = Vec::new();
            loop {
                let id_tok = self.expect(TokenType::Identifier)?;
                syms.push(id_tok.lexeme);
                if self.match_one(TokenType::Comma) {
                    continue;
                } else {
                    break;
                }
            }
            self.expect(TokenType::RightBrace)?;
            Ok((path, Some(syms)))
        } else {
            Ok((path, None))
        }
    }

    fn parse_function(&mut self) -> ParseResult<FunctionDecl> {
        let mut attributes = Vec::new();
        let mut visibility = Visibility::Public; // Default visibility
        let mut modifiers = Vec::new();

        // Parse attributes
        while self.peek().token_type == TokenType::Attribute
            || self.peek().token_type == TokenType::ParameterizedAttribute
        {
            attributes.push(self.advance().lexeme.clone());
        }

        // Parse visibility modifier
        if self.match_tnv(TokenType::Keyword, "public") {
            visibility = Visibility::Public;
        } else if self.match_tnv(TokenType::Keyword, "private") {
            visibility = Visibility::Private;
        }
        // Add other visibility modifiers...

        // Parse async modifier
        let mut is_async = false;
        if self.match_tnv(TokenType::Keyword, "async") {
            modifiers.push(FunctionModifier::Async);
            is_async = true;
        }

        // Parse function keyword (check for multidispatch)
        let fn_tok = self.expect(TokenType::Keyword)?;

        if self.match_tnv(TokenType::Operator, "+") {
            modifiers.push(FunctionModifier::Multidispatch);
        }

        // Parse function name (allow keywords as identifiers, e.g. fn merge)
        let name_tok = self.expect_ident_like()?;
        let name = name_tok.lexeme.clone();

        // Parse generics
        let generics = if self.match_tnv(TokenType::Operator, "<") {
            self.parse_generics()?
        } else {
            Vec::new()
        };

        // params
        self.expect(TokenType::LeftParen)?;
        let mut params = Vec::new();
        while self.peek().token_type != TokenType::RightParen {
            let pattern = self.parse_pattern()?; // Changed from expect Identifier
            let mut typ = None;
            if self.match_one(TokenType::Colon) {
                typ = Some(self.parse_type()?);
            }
            let mut default = None;
            if self.match_tnv(TokenType::Operator, "=") {
                default = Some(self.parse_expression(0)?);
            }
            params.push(Param {
                pattern,
                typ,
                default,
            });
            if self.match_one(TokenType::Comma) {
                continue;
            } else {
                break;
            }
        }
        self.expect(TokenType::RightParen)?;

        // Parse return type
        let mut ret_type = None;
        if self.match_one(TokenType::FatArrow) {
            ret_type = Some(self.parse_type()?);
        }

        // Parse effects
        let effects = if self.match_one(TokenType::EffectMarker) {
            self.parse_effects()?
        } else {
            Vec::new()
        };

        // Parse where clauses
        let where_clauses = if self.match_tnv(TokenType::Keyword, "where") {
            self.parse_where_clauses()?
        } else {
            Vec::new()
        };

        // Parse body
        let body = if self.match_one(TokenType::ShortArrow) {
            // self.advance();
            Some(Block {
                stmts: vec![Stmt::Return(Some(self.parse_expression(0).unwrap()))],
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

    fn parse_generics(&mut self) -> ParseResult<Vec<GenericParam>> {
        let mut generics = Vec::new();

        while !self.is_at_end() && !self.match_gt() {
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
                continue;
            }
        }

        Ok(generics)
    }

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

    fn parse_type_constraints(&mut self) -> ParseResult<Vec<TypeConstraint>> {
        let mut constraints = Vec::new();

        while !self.is_at_end() {
            let tok = self.peek().clone();

            match tok.token_type {
                TokenType::Identifier => {
                    let ident = self.advance().lexeme.clone();

                    // Check for trait bounds or other constraints
                    if self.match_tnv(TokenType::Operator, "=") {
                        // Type equality constraint: T = Type
                        let type_ref = self.parse_type()?;
                        constraints.push(TypeConstraint::TypeEq(ident, type_ref.name));
                    } else if self.match_one(TokenType::Colon) {
                        // Trait bound: T: Trait
                        let trait_name = self.expect(TokenType::Identifier)?.lexeme;
                        constraints.push(TypeConstraint::TraitBound(format!(
                            "{}:{}",
                            ident, trait_name
                        )));
                    } else if self.match_tnv(TokenType::Keyword, "subtype") {
                        // Subtype constraint: T subtype of U
                        self.expect(TokenType::Keyword)?; // "of"
                        let supertype = self.parse_type()?;
                        constraints.push(TypeConstraint::SubtypeOf(supertype.name));
                    } else {
                        // Simple trait bound
                        constraints.push(TypeConstraint::TraitBound(ident));
                    }
                }
                TokenType::Keyword if tok.lexeme == "lifetime" => {
                    // Lifetime bound: 'a
                    self.advance();
                    let lifetime = self.expect(TokenType::Identifier)?.lexeme;
                    constraints.push(TypeConstraint::LifetimeBound(lifetime));
                }
                _ => break,
            }

            // Check for additional constraints
            if !self.match_tnv(TokenType::Operator, "+") {
                break;
            }
        }

        Ok(constraints)
    }

    fn parse_effect_params(&mut self) -> ParseResult<Vec<TypeRef>> {
        let mut params = Vec::new();

        while self.peek().token_type != TokenType::RightParen && !self.is_at_end() {
            let type_ref = self.parse_type()?;
            params.push(type_ref);

            if self.match_one(TokenType::Comma) {
                continue;
            } else {
                break;
            }
        }

        Ok(params)
    }

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

    fn parse_struct(&mut self) -> ParseResult<StructDecl> {
        self.expect(TokenType::Keyword)?; // 'struct'
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();
        self.expect(TokenType::LeftBrace)?;
        let mut fields = Vec::new();
        while self.peek().token_type != TokenType::RightBrace {
            let field_name_tok = self.expect(TokenType::Identifier)?;
            let field_name = field_name_tok.lexeme.clone();
            self.expect(TokenType::Colon)?;
            let typ = self.parse_type()?;
            fields.push((field_name, typ));
            if self.match_one(TokenType::Comma) {
                continue;
            } else {
                break;
            }
        }
        self.expect(TokenType::RightBrace)?;
        Ok(StructDecl { name, fields })
    }

    fn parse_enum(&mut self) -> ParseResult<EnumDecl> {
        self.expect(TokenType::Keyword)?; // 'enum'
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
                let mut fields = Vec::new();
                while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
                    let field_name_tok = self.expect(TokenType::Identifier)?;
                    self.expect(TokenType::Colon)?;
                    let typ = self.parse_type()?;
                    fields.push(EnumVariantField {
                        name: Some(field_name_tok.lexeme),
                        typ,
                    });

                    if self.match_one(TokenType::Comma) {
                        if self.peek().token_type == TokenType::RightBrace {
                            break;
                        }
                        continue;
                    }

                    if self.peek().token_type == TokenType::RightBrace {
                        break;
                    }

                    return Err(ParseError::UnexpectedToken {
                        expected: "Comma or RightBrace".into(),
                        found: self.peek().clone(),
                        idx: self.idx,
                    });
                }
                self.expect(TokenType::RightBrace)?;
                VariantKind::Struct(fields)
            } else if self.peek().token_type == TokenType::LeftParen {
                self.advance();
                let mut fields = Vec::new();
                while self.peek().token_type != TokenType::RightParen && !self.is_at_end() {
                    let is_named = self.peek().token_type == TokenType::Identifier
                        && matches!(
                            self.tokens.get(self.idx + 1),
                            Some(t) if t.token_type == TokenType::Colon
                        );

                    if is_named {
                        let field_name = self.expect(TokenType::Identifier)?.lexeme;
                        self.expect(TokenType::Colon)?;
                        let typ = self.parse_type()?;
                        fields.push(EnumVariantField {
                            name: Some(field_name),
                            typ,
                        });
                    } else {
                        let typ = self.parse_type()?;
                        fields.push(EnumVariantField { name: None, typ });
                    }

                    if self.match_one(TokenType::Comma) {
                        if self.peek().token_type == TokenType::RightParen {
                            break;
                        }
                        continue;
                    }

                    if self.peek().token_type == TokenType::RightParen {
                        break;
                    }

                    return Err(ParseError::UnexpectedToken {
                        expected: "Comma or RightParen".into(),
                        found: self.peek().clone(),
                        idx: self.idx,
                    });
                }
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
            name,
            generics,
            variants,
        })
    }

    fn parse_trait(&mut self) -> ParseResult<TraitDecl> {
        // We already matched the "trait" keyword in parse_item
        self.expect(TokenType::Keyword)?; // 'trait'
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        self.expect(TokenType::LeftBrace)?;
        let mut methods = Vec::new();

        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let func = self.parse_function()?;
            methods.push(func);
        }

        self.expect(TokenType::RightBrace)?;

        Ok(TraitDecl { name, methods })
    }

    fn parse_impl(&mut self) -> ParseResult<ImplDecl> {
        // We already matched the "impl" keyword in parse_item
        self.expect(TokenType::Keyword)?; // 'impl'

        // First type after `impl` can be either the target type (inherent impl)
        // or the trait name (trait impl). We parse a full TypeRef and
        // then check for a following `for` keyword.
        let first_type = self.parse_type()?;

        let (trait_name, target) = if self.match_tnv(TokenType::Keyword, "for") {
            // Trait impl: `impl Trait for Type { ... }`
            // We only store the trait's name string in the AST.
            let trait_name = Some(first_type.name.clone());
            let target = self.parse_type()?;
            (trait_name, target)
        } else {
            // Inherent impl: `impl Type { ... }`
            (None, first_type)
        };

        self.expect(TokenType::LeftBrace)?;
        let mut methods = Vec::new();

        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let func = self.parse_function()?;
            methods.push(func);
        }

        self.expect(TokenType::RightBrace)?;

        Ok(ImplDecl {
            trait_name,
            target,
            methods,
        })
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        self.expect(TokenType::LeftBrace)?;
        let mut stmts = Vec::new();
        let mut let_names = Vec::new(); // Track variable names defined with let
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let s = self.parse_statement()?;
            // If this is a let statement, track the variable name for auto-delete
            // Borrow `s` here so we don't move out of it before pushing to `stmts`.
            if let Stmt::Let { pattern, .. } = &s {
                // Extract identifier from pattern if it's a simple identifier
                if let PatternKind::Identifier(name) = &pattern.kind {
                    let_names.push(name.clone());
                }
            }
            stmts.push(s);
        }
        self.expect(TokenType::RightBrace)?;
        // Auto-delete variables defined in this scope
        for name in let_names {
            stmts.push(Stmt::Delete {
                expr: Expr::Ident(name),
            });
        }
        Ok(Block { stmts })
    }

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

    fn parse_watch_variable_expr(&mut self) -> ParseResult<Expr> {
        // Avoid ambiguity with struct literal parsing: in `watch a, b { ... }`,
        // the `{` belongs to the watch body, not to `b { ... }`.
        let tok = self.peek().clone();
        if matches!(
            tok.token_type,
            TokenType::Identifier | TokenType::ModulePath
        ) && matches!(
            self.tokens.get(self.idx + 1),
            Some(t) if t.token_type == TokenType::LeftBrace
        ) {
            let ident = self.advance().lexeme.clone();
            let expr = Expr::Ident(ident);
            return self.parse_postfix(expr);
        }

        self.parse_expression(0)
    }

    fn parse_expr_no_struct_literal_before_block(&mut self) -> ParseResult<Expr> {
        // Used in contexts like `in context <target> { ... }` where
        // `<target> { ... }` must NOT be parsed as a struct literal.
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

        self.parse_expression(0)
    }

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

        // optional semicolon
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
            "let" => self.parse_let_stmt(), // uses separated method 'cuz there are other 'let' usages eg. in joins stmt
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
                // Use special helper to avoid interpreting `<iter> {` as a struct literal
                // in constructs like `for x in xs { ... }`.
                let iter = self.parse_expr_no_struct_literal_before_block()?;

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
                    let expr = self.parse_expression(0)?;
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
                    variables.push(self.parse_watch_variable_expr()?);
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
                self.expect(TokenType::Keyword)?; // 'atomically'
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

                // optional semicolon, 'cuz this one doesn't have block
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
                // expression statement
                let expr = self.parse_expression(0)?;
                if self.peek().token_type == TokenType::Semicolon {
                    self.advance();
                }
                Ok(Stmt::Expr(expr))
            }
        }
    }

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
        let target = self.parse_expr_no_struct_literal_before_block()?;
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

    // ---- Expression parsing (Pratt-style with precedence table) ----
    // This is simplified: operator detection uses token.lexeme for operator symbol,
    // and uses token.token_type for categories where helpful.
    fn parse_expression(&mut self, min_prec: u8) -> ParseResult<Expr> {
        let mut left = self.parse_unary_or_primary()?;

        loop {
            // if at end or token isn't an operator, break
            let op_tok = self.peek().clone();
            let (prec, right_assoc) = match op_precedence(&op_tok) {
                Some((p, ra)) => (p, ra),
                None => break,
            };

            if prec < min_prec {
                break;
            }

            // consume operator
            let op_lex = self.advance().lexeme.clone();

            if op_lex == "?" {
                left = Expr::Try(Box::new(left));
                continue;
            }

            let next_min = if right_assoc { prec } else { prec + 1 };
            let right = self.parse_expression(next_min)?;

            // Handle pipeline operator specially
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

    // Update parse_unary_or_primary to include postfix parsing
    fn parse_unary_or_primary(&mut self) -> ParseResult<Expr> {
        // Handle unary operators first
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
                let rhs = self.parse_expression(100)?; // high prec for unary
                return Ok(Expr::Unary {
                    op,
                    rhs: Box::new(rhs),
                });
            }
        }

        // Parse primary expression and then apply postfix operations
        let primary = self.parse_primary()?;
        self.parse_postfix(primary)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let tok = self.peek().clone();
        match tok.token_type {
            TokenType::LambdaArrow => {
                self.advance(); // consume '\\'
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
                    // Struct literal: Ident { field: expr, ... }
                    self.advance(); // consume '{'
                    let mut fields = Vec::new();
                    while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
                        let field_name = self.expect(TokenType::Identifier)?.lexeme;
                        self.expect(TokenType::Colon)?;
                        let field_expr = self.parse_expression(0)?;
                        fields.push((field_name, field_expr));
                        if self.match_one(TokenType::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenType::RightBrace)?;
                    Ok(Expr::Literal(Literal::Struct {
                        name: ident,
                        base: None,
                        fields,
                    }))
                } else if ident == "v" && self.peek().token_type == TokenType::LeftBracket {
                    // Vector literal: v[expr, expr, ...]
                    self.advance(); // consume '['
                    let mut elements = Vec::new();
                    while !self.is_at_end() && self.peek().token_type != TokenType::RightBracket {
                        let elem = self.parse_expression(0)?;
                        elements.push(elem);
                        if self.match_one(TokenType::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenType::RightBracket)?;
                    Ok(Expr::Literal(Literal::Vector(elements)))
                } else {
                    Ok(Expr::Ident(ident))
                }
            }
            TokenType::LeftParen => {
                self.advance(); // consume '('
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
                self.advance(); // consume '['
                if self.peek().token_type == TokenType::RightBracket {
                    self.advance();
                    return Ok(Expr::Literal(Literal::Array(Vec::new())));
                }

                let first_expr = self.parse_expression(0)?;

                // List comprehension: [expr | pattern <- iter, guard]
                if self.match_one(TokenType::Pipe) {
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
                    // Normal array literal: [a, b, c]
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

    fn parse_postfix(&mut self, left: Expr) -> ParseResult<Expr> {
        let mut expr = left;
        loop {
            match self.peek().token_type {
                TokenType::Dot => {
                    self.advance(); // consume '.'
                    let field_or_method = self.expect(TokenType::Identifier)?.lexeme;
                    let mut generics = Vec::new();
                    if self.peek().token_type == TokenType::Operator && self.peek().lexeme == "<" {
                        self.advance(); // <
                        if self.peek().token_type != TokenType::Operator
                            || self.peek().lexeme != ">"
                        {
                            loop {
                                let typ = self.parse_type()?;
                                generics.push(typ);
                                if self.match_one(TokenType::Comma) {
                                    continue;
                                } else {
                                    break;
                                }
                            }
                        }
                        self.expect_nv(TokenType::Operator, ">")?; // >
                    }
                    if self.peek().token_type == TokenType::LeftParen {
                        // Method call (with or without generics)
                        self.advance(); // (
                        let args = self.parse_arguments()?;
                        self.expect(TokenType::RightParen)?;
                        expr = Expr::MethodCall {
                            object: Box::new(expr),
                            method: field_or_method,
                            generics,
                            args,
                        };
                    } else {
                        // Field access (error if generics were parsed but no call)
                        if !generics.is_empty() {
                            return Err(ParseError::Generic(
                                "Unexpected generics without method call".into(),
                            ));
                        }
                        expr = Expr::FieldAccess {
                            object: Box::new(expr),
                            field: field_or_method,
                        };
                    }
                }
                TokenType::LeftBracket => {
                    // Index access
                    self.advance(); // [
                    let index_expr = self.parse_expression(0)?;
                    self.expect(TokenType::RightBracket)?;
                    expr = Expr::Index {
                        array: Box::new(expr),
                        index: Box::new(index_expr),
                    };
                }
                TokenType::Operator if self.peek().lexeme == "<" => {
                    // Disambiguate generic calls from comparison `<`.
                    // Only treat `<...>` as generics if we can find a matching `>`
                    // that is immediately followed by a `(`.
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
                            }
                        } else {
                            break;
                        }
                    }

                    if !found_generic_call {
                        break;
                    }

                    self.advance(); // <
                    let mut generics = Vec::new();
                    if self.peek().token_type != TokenType::Operator || self.peek().lexeme != ">" {
                        loop {
                            let typ = self.parse_type()?;
                            generics.push(typ);
                            if self.match_one(TokenType::Comma) {
                                continue;
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect_nv(TokenType::Operator, ">")?; // >
                                                               // Expect call parentheses after generics
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
                    // Function call without generics
                    self.advance(); // (
                    let args = self.parse_arguments()?;
                    self.expect(TokenType::RightParen)?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        generics: Vec::new(),
                        args,
                    };
                }
                TokenType::Keyword if self.peek().lexeme == "with" => {
                    // Record update: expr with { fields }
                    // Disambiguate from constructs like `poll <expr> with interval=...`.
                    if !matches!(
                        self.tokens.get(self.idx + 1),
                        Some(t) if t.token_type == TokenType::LeftBrace
                    ) {
                        break;
                    }

                    self.advance(); // consume 'with'
                    self.expect(TokenType::LeftBrace)?;

                    let mut fields = Vec::new();
                    while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
                        let field_name = self.expect(TokenType::Identifier)?.lexeme;
                        self.expect(TokenType::Colon)?;
                        let field_expr = self.parse_expression(0)?;
                        fields.push((field_name, field_expr));
                        if self.match_one(TokenType::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenType::RightBrace)?;

                    expr = Expr::Literal(Literal::Struct {
                        name: "update".to_string(), // Generic name for updates
                        base: Some(Box::new(expr)),
                        fields,
                    });
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    // Add parse_arguments function
    fn parse_arguments(&mut self) -> ParseResult<Vec<Expr>> {
        let mut args = Vec::new();
        while self.peek().token_type != TokenType::RightParen && !self.is_at_end() {
            let arg = self.parse_expression(0)?;
            args.push(arg);
            if self.match_one(TokenType::Comma) {
                continue;
            } else {
                break;
            }
        }
        Ok(args)
    }

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

        // Parse base type name
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

        // Parse generics if present
        if self.match_tnv(TokenType::Operator, "<") {
            loop {
                let generic_type = self.parse_type()?;
                generics.push(generic_type);

                if self.match_one(TokenType::Comma) {
                    continue;
                }
                break;
            }
            self.expect_gt()?;
        }

        // Parse nullable modifier
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

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let mut bindings = Vec::new();
        let tok = self.peek().clone();
        let original_kind = match tok.token_type {
            TokenType::LeftParen => {
                // Tuple pattern: (pat1, pat2, ...)
                self.advance();
                let mut patterns = Vec::new();
                while !self.is_at_end() && self.peek().token_type != TokenType::RightParen {
                    let sub_pat = self.parse_pattern()?;
                    bindings.extend(sub_pat.bindings.clone()); // Collect sub-bindings
                    patterns.push(sub_pat);
                    if self.match_one(TokenType::Comma) {
                        continue;
                    } else {
                        break;
                    }
                }
                self.expect(TokenType::RightParen)?;
                PatternKind::Tuple(patterns)
            }
            TokenType::LeftBracket => {
                // Array pattern: [pat1, pat2, ...]
                self.advance();
                let mut patterns = Vec::new();
                while !self.is_at_end() && self.peek().token_type != TokenType::RightBracket {
                    let sub_pat = self.parse_pattern()?;
                    bindings.extend(sub_pat.bindings.clone()); // Collect sub-bindings
                    patterns.push(sub_pat);
                    if self.match_one(TokenType::Comma) {
                        continue;
                    } else {
                        break;
                    }
                }
                self.expect(TokenType::RightBracket)?;
                PatternKind::Array(patterns)
            }
            TokenType::LeftBrace => {
                // Struct pattern: {field1: pat1, field2: pat2} หรือ {field1, field2} (shorthand)
                self.advance();
                let mut fields = Vec::new();

                while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
                    let field_name = self.expect(TokenType::Identifier)?.lexeme;

                    // ตรวจสอบว่าเป็นรูปแบบเต็ม {field: pattern} หรือ shorthand {field}
                    if self.match_one(TokenType::Colon) {
                        // รูปแบบเต็ม: {field: pattern}
                        let field_pat = self.parse_pattern()?;
                        bindings.extend(field_pat.bindings.clone()); // Collect sub-bindings
                        fields.push((field_name, field_pat));
                    } else {
                        // รูปแบบ shorthand: {field} - สร้าง pattern เป็น Identifier(field)
                        let field_pat = Pattern {
                            kind: PatternKind::Identifier(field_name.clone()),
                            bindings: vec![field_name.clone()],
                        };
                        bindings.push(field_name.clone()); // เพิ่ม binding
                        fields.push((field_name, field_pat));
                    }

                    if self.match_one(TokenType::Comma) {
                        continue;
                    } else {
                        break;
                    }
                }
                self.expect(TokenType::RightBrace)?;
                PatternKind::Struct { name: None, fields }
            }
            TokenType::Identifier => {
                let ident = self.advance().lexeme.clone();
                let is_upper = ident.chars().next().map_or(false, |c| c.is_uppercase());
                if ident == "_" {
                    PatternKind::Wildcard
                } else if ident == "Nil" {
                    PatternKind::Nil
                } else if ident == "None" {
                    PatternKind::NoneVariant
                } else if ident == "Some" && self.peek().token_type == TokenType::LeftParen {
                    self.advance(); // Consume '('
                    let inner_pat = self.parse_pattern()?;
                    bindings.extend(inner_pat.bindings.clone());
                    self.expect(TokenType::RightParen)?;
                    PatternKind::SomeVariant(Box::new(inner_pat))
                } else if ident == "Ok" && self.peek().token_type == TokenType::LeftParen {
                    self.advance(); // Consume '('
                    let inner_pat = self.parse_pattern()?;
                    bindings.extend(inner_pat.bindings.clone());
                    self.expect(TokenType::RightParen)?;
                    PatternKind::OkVariant(Box::new(inner_pat))
                } else if ident == "Err" && self.peek().token_type == TokenType::LeftParen {
                    self.advance(); // Consume '('
                    let inner_pat = self.parse_pattern()?;
                    bindings.extend(inner_pat.bindings.clone());
                    self.expect(TokenType::RightParen)?;
                    PatternKind::ErrVariant(Box::new(inner_pat))
                } else if self.peek().token_type == TokenType::LeftBrace {
                    // Struct pattern or enum struct variant
                    self.advance(); // Consume '{'
                    let mut fields = Vec::new();

                    while !self.is_at_end() && self.peek().token_type != TokenType::RightBrace {
                        let field_name = self.expect(TokenType::Identifier)?.lexeme;

                        // ตรวจสอบว่าเป็นรูปแบบเต็มหรือ shorthand
                        if self.match_one(TokenType::Colon) {
                            // รูปแบบเต็ม: field: pattern
                            let field_pat = self.parse_pattern()?;
                            bindings.extend(field_pat.bindings.clone()); // Collect
                            fields.push((field_name, field_pat));
                        } else {
                            // รูปแบบ shorthand: field - สร้าง pattern เป็น Identifier(field)
                            let field_pat = Pattern {
                                kind: PatternKind::Identifier(field_name.clone()),
                                bindings: vec![field_name.clone()],
                            };
                            bindings.push(field_name.clone()); // เพิ่ม binding
                            fields.push((field_name, field_pat));
                        }

                        if self.match_one(TokenType::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenType::RightBrace)?;
                    if is_upper {
                        let inner = Some(Box::new(Pattern {
                            kind: PatternKind::Struct { name: None, fields },
                            bindings: bindings.clone(),
                        }));
                        PatternKind::EnumVariant {
                            variant_name: ident,
                            inner_pattern: inner,
                        }
                    } else {
                        PatternKind::Struct {
                            name: Some(ident),
                            fields,
                        }
                    }
                } else if self.peek().token_type == TokenType::LeftParen {
                    // Variant positional: Ident(pat1, pat2)
                    self.advance(); // Consume '('
                    let mut patterns = Vec::new();
                    let mut sub_bindings = Vec::new();
                    while !self.is_at_end() && self.peek().token_type != TokenType::RightParen {
                        let sub_pat = self.parse_pattern()?;
                        sub_bindings.extend(sub_pat.bindings.clone());
                        patterns.push(sub_pat);
                        if self.match_one(TokenType::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenType::RightParen)?;
                    let inner = if patterns.is_empty() {
                        return Err(ParseError::Generic("Empty tuple in variant".into()));
                    } else if patterns.len() == 1 {
                        Some(Box::new(patterns.remove(0)))
                    } else {
                        Some(Box::new(Pattern {
                            kind: PatternKind::Tuple(patterns),
                            bindings: sub_bindings,
                        }))
                    };
                    bindings = inner.as_ref().map_or(Vec::new(), |p| p.bindings.clone());
                    PatternKind::EnumVariant {
                        variant_name: ident,
                        inner_pattern: inner,
                    }
                } else if is_upper {
                    // Unit enum variant
                    PatternKind::EnumVariant {
                        variant_name: ident,
                        inner_pattern: None,
                    }
                } else {
                    bindings.push(ident.clone());
                    PatternKind::Identifier(ident)
                }
            }
            TokenType::IntLiteral
            | TokenType::FloatLiteral
            | TokenType::StringLiteral
            | TokenType::BoolLiteral => {
                let literal = match self.parse_primary()? {
                    Expr::Literal(l) => l,
                    _ => return Err(ParseError::Generic("Expected literal".into())),
                };
                PatternKind::Literal(literal)
            }
            _ => return Err(ParseError::Generic("Expected pattern".into())),
        };

        let original_bindings = bindings;

        let mut total_bindings = original_bindings.clone();

        let mut or_patterns = vec![Pattern {
            kind: original_kind,
            bindings: original_bindings,
        }];

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

    fn parse_lambda(&mut self) -> ParseResult<Expr> {
        // Parse parameters: \param :> expr or \param1, param2 :> expr
        let mut params = Vec::new();

        // Parse first parameter
        let param_name = self.expect(TokenType::Identifier)?.lexeme;
        params.push(Param {
            pattern: Pattern {
                kind: PatternKind::Identifier(param_name.clone()),
                bindings: vec![param_name],
            },
            typ: None,
            default: None,
        });

        // Parse additional parameters if comma-separated
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

        // Expect the lambda arrow :>
        self.expect(TokenType::ShortArrow)?;

        // Parse the lambda body
        let body = self.parse_expression(0)?;

        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
        })
    }
}

// operator precedence table
// Returns (precedence, right_associative)
pub fn op_precedence(tok: &Token) -> Option<(u8, bool)> {
    match tok.lexeme.as_str() {
        "||" | "or" => Some((1, false)),
        "&&" | "and" => Some((2, false)),
        "==" | "!=" | "<" | "<=" | ">" | ">=" => Some((3, false)),
        "|>" => Some((11, false)), // pipeline operator (high precedence, left-assoc)
        "|" => Some((4, false)), // bitwise or
        "^" => Some((5, false)),
        "&" => Some((6, false)),
        "<<" | ">>" => Some((7, false)),
        "+" | "-" => Some((8, false)),
        "*" | "/" | "%" => Some((9, false)),
        "^." /* some custom op */ => Some((10, false)),
        "::" => Some((12, false)),
        "?" => Some((13, false)),
        "=" | ":=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" => Some((0, true)), // assignments (lowest precedence, right-assoc)
        _ => None,
    }
}
