#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(dead_code)]

// parser.rs
use crate::tokenizer::{Token, TokenType};
use std::fmt;
use crate::ptypes::*;

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
                                                              // Item::Class(inner) => write!(f, "{:#?}", inner),
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

    // top-level parse entry
    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut items = Vec::new();
        while !self.is_at_end() {
            let item = self.parse_item()?;
            items.push(item);
        }
        Ok(Program { items })
    }

    fn parse_item(&mut self) -> ParseResult<Item> {
        let kw = self.peek().lexeme.clone();
        match &self.peek().token_type {
            TokenType::Keyword => {
                // Need to inspect lexeme to decide which keyword it is
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
                        let func = self.parse_function()?;
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
                    // "class" => {
                    //     // self.advance();
                    //     // let class = self.parse_class()?;
                    //     // // Ok(Item::Class(class))
                    // }
                    _ => {
                        // fallback: treat as expression stmt or error
                        let stmt = self.parse_statement()?;
                        // wrap into functionless top-level expression (not ideal but acceptable)
                        if let Stmt::Expr(expr) = stmt {
                            // create a synthetic function? we'll return Generic item by expression — but for now error.
                            Err(ParseError::Generic(format!(
                                "Unsupported top-level keyword: {}",
                                kw
                            )))
                        } else {
                            Err(ParseError::Generic(format!(
                                "Unsupported top-level keyword: {}",
                                kw
                            )))
                        }
                    }
                }
            }
            TokenType::Attribute => {
                let func = self.parse_function()?;
                Ok(Item::Function(func))
            }
            _ => Err(ParseError::Generic(String::from(format!(
                "Unsupported module-only scripts, founded {}",
                &self.peek()
            )))),
        }
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
        while self.peek().token_type == TokenType::Attribute {
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

        // Parse function name
        let name_tok = self.expect(TokenType::Identifier)?;
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

        while !self.is_at_end() && !self.match_tnv(TokenType::Operator, ">") {
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
                    fields.push((field_name_tok.lexeme, typ));
                    if self.peek().token_type == TokenType::RightBrace {
                        break;
                    }
                    self.expect(TokenType::Comma)?;
                }
                self.expect(TokenType::RightBrace)?;
                VariantKind::Struct(fields)
            } else if self.peek().token_type == TokenType::LeftParen {
                self.advance();
                let mut types = Vec::new();
                while self.peek().token_type != TokenType::RightParen && !self.is_at_end() {
                    let typ = self.parse_type()?;
                    types.push(typ);
                    if self.peek().token_type == TokenType::RightParen {
                        break;
                    }
                    self.expect(TokenType::Comma)?;
                }
                self.expect(TokenType::RightParen)?;
                VariantKind::Tuple(types)
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

    fn parse_block(&mut self) -> ParseResult<Block> {
        self.expect(TokenType::LeftBrace)?;
        let mut stmts = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            let s = self.parse_statement()?;
            stmts.push(s);
        }
        self.expect(TokenType::RightBrace)?;
        Ok(Block { stmts })
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

    fn parse_statement(&mut self) -> ParseResult<Stmt> {
        match self.peek().lexeme.as_str() {
            "let" => self.parse_let_stmt(), // uses separated method 'cuz there are other 'let' usages eg. in joins stmt
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
                self.expect(TokenType::Keyword)?; // expect 'in'
                let iter = self.parse_expression(0)?;
                let body = self.parse_block()?;
                Ok(Stmt::For { pat, iter, body })
            }
            "match" => {
                self.expect(TokenType::Keyword)?; // 'match'
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
                self.expect(TokenType::Keyword)?;

                let mut variables = Vec::new();
                loop {
                    variables.push(self.parse_expression(0)?);
                    if self.match_one(TokenType::Comma) {
                        continue;
                    } else {
                        break;
                    }
                }

                self.expect(TokenType::LeftBrace)?;

                let mut clauses = Vec::new();
                while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
                    self.expect(TokenType::Keyword)?; // 'when'
                    let condition = self.parse_expression(0)?;
                    self.expect(TokenType::ShortArrow)?;
                    let body = self.parse_block()?;

                    clauses.push(WatchClause { condition, body });
                }

                self.expect(TokenType::RightBrace)?;
                Ok(Stmt::Watch { variables, clauses })
            }
            "converge" => {
                self.expect(TokenType::Keyword)?; // 'converge'
                self.expect(TokenType::Keyword)?; // 'with'

                let variable = self.expect(TokenType::Identifier)?.lexeme;

                let body = self.parse_block()?;
                self.expect(TokenType::Keyword)?; // 'until'
                let until = self.parse_expression(0)?;

                Ok(Stmt::Converge {
                    variable,
                    body,
                    until,
                })
            }
            "within" => {
                self.expect(TokenType::Keyword)?; // 'within'
                let time = self.parse_expression(0)?;

                let condition = if self.match_tnv(TokenType::Keyword, "if") {
                    Some(self.parse_expression(0)?)
                } else {
                    None
                };

                self.expect(TokenType::ShortArrow)?;
                let body = self.parse_block()?;

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
                self.expect(TokenType::Keyword)?; // 'trap'
                let error_condition = self.parse_expression(0)?;
                self.expect(TokenType::ShortArrow)?;
                let body = self.parse_block()?;
                Ok(Stmt::Trap {
                    error_condition,
                    body,
                })
            }
            "guard" => {
                self.expect(TokenType::Keyword)?; // 'guard'
                let condition = self.parse_expression(0)?;
                self.expect(TokenType::ShortArrow)?;
                let then_block = self.parse_block()?;

                let else_block = if self.match_tnv(TokenType::Keyword, "else") {
                    self.expect(TokenType::ShortArrow)?;
                    Some(self.parse_block()?)
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
                self.expect(TokenType::Keyword)?; // 'poll'
                let signal = self.parse_expression(0)?;
                self.expect(TokenType::Keyword)?; // 'with'
                self.expect(TokenType::Keyword)?; // 'interval'
                self.expect(TokenType::Operator)?; // '='
                let interval = self.parse_expression(0)?;
                self.expect(TokenType::ShortArrow)?;
                let body = self.parse_block()?;

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

            let next_min = if right_assoc { prec } else { prec + 1 };
            let right = self.parse_expression(next_min)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op_lex,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    // Update parse_unary_or_primary to include postfix parsing
    fn parse_unary_or_primary(&mut self) -> ParseResult<Expr> {
        // Handle unary operators first
        let tok = self.peek().clone();
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
            TokenType::IntLiteral => {
                self.advance();
                Ok(Expr::Literal(Literal::Int(tok.lexeme)))
            }
            TokenType::FloatLiteral => {
                self.advance();
                Ok(Expr::Literal(Literal::Float(tok.lexeme)))
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
            TokenType::Identifier | TokenType::ModulePath => {
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
                Ok(Expr::Literal(Literal::Array(elements)))
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
            while !self.is_at_end() && !self.match_tnv(TokenType::Operator, ">") {
                let generic_type = self.parse_type()?;
                generics.push(generic_type);

                if self.peek().token_type == TokenType::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
            self.expect_nv(TokenType::Operator, ">")?;
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
}

// operator precedence table
// Returns (precedence, right_associative)
pub fn op_precedence(tok: &Token) -> Option<(u8, bool)> {
    match tok.lexeme.as_str() {
        "||" | "or" => Some((1, false)),
        "&&" | "and" => Some((2, false)),
        "==" | "!=" | "<" | "<=" | ">" | ">=" => Some((3, false)),
        "|" => Some((4, false)), // pipeline or bitwise
        "^" => Some((5, false)),
        "&" => Some((6, false)),
        "<<" | ">>" => Some((7, false)),
        "+" | "-" => Some((8, false)),
        "*" | "/" | "%" => Some((9, false)),
        "^." /* some custom op */ => Some((10, false)),
        "::" => Some((12, false)),
        "=" | ":=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" => Some((0, true)), // assignments (lowest precedence, right-assoc)
        _ => None,
    }
}
