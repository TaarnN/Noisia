// parser.rs
// A pragmatic, extendable parser for Noisia-like syntax.
// Depends on your tokenizer.rs providing `Token` and `TokenType`.
use crate::tokenizer::{Token, TokenType};
use std::fmt;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: String,
        found: Token,
        idx: usize,
    },
    EOF {
        message: String,
    },
    Generic(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                idx,
            } => write!(
                f,
                "Parse error at {}:{}: expected {}, found {}",
                found.line, found.column, expected, found
            ),
            ParseError::EOF { message } => write!(f, "Unexpected EOF: {}", message),
            ParseError::Generic(s) => write!(f, "Parse error: {}", s),
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

//
// AST (minimal, extendable)
//
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    ModuleDecl(ModuleDecl),
    Import(ImportDecl),
    Function(FunctionDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Trait(TraitDecl),
    Impl(ImplDecl),
    Class(ClassDecl),
    // ... add other top-level items: trait, impl, class, macro, etc.
}

#[derive(Debug, Clone)]
pub struct ModuleDecl {
    pub path: String,
}

#[derive(Debug, Clone)]
pub struct ImportDecl {
    pub path: String,
    pub symbols: Option<Vec<String>>, // `import A::{x,y}` or `import A::B`
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub ret_type: Option<TypeRef>,
    pub body: Option<Block>,
    pub is_async: bool,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub typ: Option<TypeRef>,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: String,
    // simple fields (name, type)
    pub fields: Vec<(String, TypeRef)>,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<String>, // simplified
}

#[derive(Debug, Clone)]
pub struct TraitDecl {
    pub name: String,
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone)]
pub struct ImplDecl {
    pub trait_name: Option<String>, // None = inherent impl, Some("Trait") = trait impl
    pub target: TypeRef,            // type name เช่น "Point"
    pub methods: Vec<FunctionDecl>, // body ของ impl
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: String,
    pub fields: Vec<FieldDecl>,
    pub constructors: Vec<ConstructorDecl>,
    pub methods: Vec<FunctionDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Protected,
    Default,
}

#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub vis: Visibility, // public / private / protected (optional)
    pub name: String,
    pub typ: TypeRef,
}

#[derive(Debug, Clone)]
pub struct ConstructorDecl {
    pub name: Option<String>, // None = primary constructor ("init"), Some("withName") = secondary
    pub params: Vec<Param>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        mutable: bool,
        typ: Option<TypeRef>,
        expr: Option<Expr>,
    },
    Expr(Expr),
    Return(Option<Expr>),
    If {
        cond: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    While {
        cond: Expr,
        body: Block,
    },
    For {
        pat: String,
        iter: Expr,
        body: Block,
    },
    // ... add more statements
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Binary {
        left: Box<Expr>,
        op: String,
        right: Box<Expr>,
    },
    Unary {
        op: String,
        rhs: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Grouping(Box<Expr>),
    // more: lambda, match, pipeline, pointer ops, etc.
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(String),
    Float(String),
    String(String),
    Bool(bool),
    Unit { v: String, u: String },
}

#[derive(Debug, Clone)]
pub struct TypeRef {
    pub name: String,           // ชื่อ type หลัก เช่น "Box", "Option", "Result"
    pub generics: Vec<TypeRef>, // พารามิเตอร์ generic เช่น [T], [K, V], [String, Int]
    pub nullable: bool,
    pub pointer_type: Option<PointerType>, // อนาคตอาจขยาย: array, slice, reference, etc.
}

#[derive(Debug, Clone, PartialEq)]
pub enum PointerType {
    RawPointer,
    ManagedPointer,
    WeakPointer,
    SharedPointer,
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
            // skip newlines/comments tokens if tokenizer emits them as tokens
            match self.peek().token_type {
                TokenType::NewLine
                | TokenType::LineComment
                | TokenType::BlockComment
                | TokenType::DocComment => {
                    self.advance();
                    continue;
                }
                _ => {}
            }

            let item = self.parse_item()?;
            items.push(item);
        }
        Ok(Program { items })
    }

    fn parse_item(&mut self) -> ParseResult<Item> {
        match &self.peek().token_type {
            TokenType::Keyword => {
                // Need to inspect lexeme to decide which keyword it is
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
            _ => {
                // Could be an expression at top-level (e.g., module-less script)
                let stmt = self.parse_statement()?;
                // For now we ignore expression-only top-levels (or could wrap into a `main` implicit)
                match stmt {
                    Stmt::Expr(expr) => {
                        ///////////////  // ignore
                        Ok(Item::Function(FunctionDecl {
                            name: "<__anon__>".into(),
                            params: vec![],
                            ret_type: None,
                            body: Some(Block {
                                stmts: vec![Stmt::Expr(expr)],
                            }),
                            is_async: false,
                        }))
                    }
                    _ => Err(ParseError::Generic(
                        "Unsupported top-level statement".into(),
                    )),
                }
            }
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
        // Accept "fn" or "async fn"
        let mut is_async = false;
        if self.peek().lexeme == "async" {
            is_async = true;
            self.advance(); // consume async
        }

        // expect fn
        let fn_tok = self.expect(TokenType::Keyword)?;
        if fn_tok.lexeme != "fn" {
            return Err(ParseError::Generic("Expected 'fn' keyword".into()));
        }

        // name
        let name_tok = self.expect(TokenType::Identifier)?;
        let name = name_tok.lexeme.clone();

        // params
        self.expect(TokenType::LeftParen)?;
        let mut params = Vec::new();
        while self.peek().token_type != TokenType::RightParen {
            let param_name_tok = self.expect(TokenType::Identifier)?;
            let param_name = param_name_tok.lexeme.clone();
            let mut typ = None;
            if self.match_one(TokenType::Colon) {
                typ = Some(self.parse_type()?);
            }
            params.push(Param {
                name: param_name,
                typ,
            });
            if self.match_one(TokenType::Comma) {
                continue;
            } else {
                break;
            }
        }
        self.expect(TokenType::RightParen)?;

        // return type: only treat FatArrow (->) as return-type marker
        let mut ret_type = None;
        if self.match_one(TokenType::FatArrow) {
            ret_type = Some(self.parse_type()?);
        }

        // body:
        // - if ShortArrow (':>') => expression body, wrap into Block with a Return or Expr stmt
        // - else if LeftBrace => full block
        // - else => no body (declaration only)
        let body = if self.match_one(TokenType::ShortArrow) {
            // expression-bodied function: parse a single expression as the body
            let expr = self.parse_expression(0)?;
            // optional semicolon after expression-body (allow both)
            if self.peek().token_type == TokenType::Semicolon {
                self.advance();
            }
            // wrap as a return stmt so downstream phases treat it uniformly
            Some(Block {
                stmts: vec![Stmt::Return(Some(expr))],
            })
        } else if self.peek().token_type == TokenType::LeftBrace {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(FunctionDecl {
            name,
            params,
            ret_type,
            body,
            is_async,
        })
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
        self.expect(TokenType::LeftBrace)?;
        let mut variants = Vec::new();
        while self.peek().token_type != TokenType::RightBrace {
            let var_tok = self.expect(TokenType::Identifier)?;
            variants.push(var_tok.lexeme.clone());
            if self.match_one(TokenType::Comma) {
                continue;
            } else {
                break;
            }
        }
        self.expect(TokenType::RightBrace)?;
        Ok(EnumDecl { name, variants })
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        self.expect(TokenType::LeftBrace)?;
        let mut stmts = Vec::new();
        while self.peek().token_type != TokenType::RightBrace && !self.is_at_end() {
            // skip newline/comments tokens
            match self.peek().token_type {
                TokenType::NewLine
                | TokenType::LineComment
                | TokenType::BlockComment
                | TokenType::DocComment => {
                    self.advance();
                    continue;
                }
                _ => {}
            }
            let s = self.parse_statement()?;
            stmts.push(s);
        }
        self.expect(TokenType::RightBrace)?;
        Ok(Block { stmts })
    }

    fn parse_statement(&mut self) -> ParseResult<Stmt> {
        // handle let/return/if/loop/while/for or expression
        match self.peek().lexeme.as_str() {
            "let" => {
                let mutable = (!self.is_at_end() && self.match_tnv(TokenType::Operator, "~"))
                    .then(|| {
                        self.advance();
                        true
                    })
                    .unwrap_or(false);
                let name_tok = self.expect(TokenType::Identifier)?;
                let name = name_tok.lexeme.clone();
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
                    name,
                    mutable,
                    typ,
                    expr,
                })
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
            "if" => {
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

    fn parse_unary_or_primary(&mut self) -> ParseResult<Expr> {
        // unary operators: `-`, `!`, `not`
        let tok = self.peek().clone();
        if matches!(tok.token_type, TokenType::Operator) {
            if tok.lexeme == "-" || tok.lexeme == "+" {
                let op = self.advance().lexeme.clone();
                let rhs = self.parse_expression(100)?; // high prec for unary
                return Ok(Expr::Unary {
                    op,
                    rhs: Box::new(rhs),
                });
            }
        }

        // primary expressions
        self.parse_primary()
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
                self.advance();
                let mut expr = Expr::Ident(tok.lexeme.clone());
                // call detection: if next is '(' => function call
                if self.peek().token_type == TokenType::LeftParen {
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    while self.peek().token_type != TokenType::RightParen {
                        let a = self.parse_expression(0)?;
                        args.push(a);
                        if self.match_one(TokenType::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }
                    self.expect(TokenType::RightParen)?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        args,
                    };
                }
                Ok(expr)
            }
            TokenType::LeftParen => {
                self.advance();
                let inner = self.parse_expression(0)?;
                self.expect(TokenType::RightParen)?;
                Ok(Expr::Grouping(Box::new(inner)))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "expression".into(),
                found: tok,
                idx: self.idx,
            }),
        }
    }

    fn parse_type(&mut self) -> ParseResult<TypeRef> {
        let mut name = String::new();
        let mut generics = Vec::new();
        let mut nullable = false;
        let mut pointer = false;
        let mut pointer_type = None;

        let mut tok = self.peek().clone();
        if tok.token_type == TokenType::Operator {
            pointer = true;
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
}

// operator precedence table
// Returns (precedence, right_associative)
pub fn op_precedence(tok: &Token) -> Option<(u8, bool)> {
    // Use token.lexeme for many operators; token.token_type can also be used.
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
        "=" | ":=" => Some((0, true)), // assignment (lowest precedence, right-assoc)
        _ => None,
    }
}
