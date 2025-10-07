use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Keywords
    Keyword,

    // Operators
    Operator,
    ShortArrow,
    FatArrow,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Semicolon,
    Colon,
    DoubleColon,
    Dot,
    TripleDot,
    Hash,
    Dollar,
    Backtick,
    Pipe,

    // Literals
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    MultilineStringLiteral,
    InterpolatedStringLiteral,
    BoolLiteral,
    UnitLiteral,

    // Identifiers
    Identifier,
    ModulePath,

    // Comments
    LineComment,
    BlockComment,
    DocComment,

    // Attributes
    Attribute,
    ParameterizedAttribute,

    // Special tokens
    LambdaArrow,
    CompileTimeKeyword,
    EffectMarker,
    IGMKeyword,

    // Error recovery
    Unknown,
    UnterminatedString,
    UnterminatedComment,
    InvalidUnit,
    // End of file
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize, column: usize) -> Self {
        Self {
            token_type,
            lexeme,
            line,
            column,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.token_type {
            _ => write!(
                f,
                "{:?}('{}') at {}:{}",
                self.token_type, self.lexeme, self.line, self.column
            ),
        }
    }
}

pub struct Tokenizer {
    input: Vec<char>,
    current: usize,
    line: usize,
    column: usize,
    keywords: HashMap<String, TokenType>,
    operators: HashMap<String, TokenType>,
}

impl Tokenizer {
    pub fn new(input: &str) -> Self {
        let mut tokenizer = Self {
            input: input.chars().collect(),
            current: 0,
            line: 1,
            column: 1,
            keywords: HashMap::new(),
            operators: HashMap::new(),
        };

        tokenizer.init_keywords();
        tokenizer.init_operators();
        tokenizer
    }

    fn init_keywords(&mut self) {
        let keywords = [
            // Core keywords
            "fn",
            "let",
            "delete",
            "mut",
            "if",
            "elif",
            "else",
            "match",
            "loop",
            "while",
            "for",
            "in",
            "break",
            "continue",
            "return",
            // Declaration keywords
            "module",
            "import",
            "struct",
            "enum",
            "trait",
            "impl",
            "class",
            "extends",
            "implements",
            "with",
            // Type keywords
            "Int",
            "Float",
            "Bool",
            "String",
            "Option",
            "Result",
            "List",
            "Self",
            // Effect & Async keywords
            "async",
            "await",
            "spawn",
            "try",
            "catch",
            "throw",
            // Access & Modifier keywords
            "public",
            "private",
            "protected",
            "internal",
            "package",
            "friend",
            "static",
            "virtual",
            "final",
            "abstract",
            "override",
            // Advanced keywords & attributes
            "subtype",
            "of",
            "macro",
            "extension",
            "mixin",
            "delegate",
            "scope",
            "defer",
            "comptime",
            "constexpr",
            "using",
            "where",
            "atomically",
            "commit",
            "rollback",
            "watch",
            "when",
            "converge",
            "until",
            "within",
            "guard",
            "panic",
            "unless",
            "poll",
            "trap",
            "on",
            "sequence",
            "igm",
            "pattern",
            "expand",
            // Boolean literals
            "true",
            "false",
            // Logical operators
            "and",
            "or",
            "not",
            // Additional keywords from your list
            "checkpoint",
            "rewind",
            "inspect",
            "temporal",
            "branch",
            "merge",
            "retry",
            "snapshot",
            "replay",
            "pause",
            "modify",
            "emit",
            "auto",
            "each",
            "trace",
            "analyze",
            "assert",
            "breakpoint",
            "at",
            "between",
            "as",
            "to",
            "latest",
            "state",
            "diff",
            "history",
            "current",
            "from",
            "that",
            "repeats",
            "batch",
            "optimize",
            "preserve",
            "skip",
            "gc",
            "effect",
            "handle",
            "contains",
            "startsWith",
            "followed_by",
            "ending",
            "is",
            "lifetime",
        ];

        for keyword in keywords.iter().copied() {
            if keyword == "true" || keyword == "false" {
                self.keywords
                    .insert(keyword.to_string(), TokenType::BoolLiteral);
            } else if keyword == "and" || keyword == "or" || keyword == "not" {
                self.keywords
                    .insert(keyword.to_string(), TokenType::Operator);
            } else if keyword == "igm" || keyword == "pattern" || keyword == "expand" {
                self.keywords
                    .insert(keyword.to_string(), TokenType::IGMKeyword);
            } else if keyword == "compile-time" || keyword == "stringify" || keyword == "emit" {
                self.keywords
                    .insert(keyword.to_string(), TokenType::CompileTimeKeyword);
            } else {
                self.keywords
                    .insert(keyword.to_string(), TokenType::Keyword);
            }
        }
    }

    fn init_operators(&mut self) {
        let operators = [
            // Multi-character operators (check these first!)
            (":::", TokenType::TripleDot),
            ("::", TokenType::DoubleColon),
            (":>", TokenType::ShortArrow),
            ("->", TokenType::FatArrow),
            ("->?", TokenType::Operator),
            ("<->", TokenType::Operator),
            ("<@", TokenType::Operator),
            ("@>", TokenType::Operator),
            ("+>", TokenType::Operator),
            ("<+", TokenType::Operator),
            ("&", TokenType::Operator),
            ("@new", TokenType::Operator),
            ("|>", TokenType::Operator),
            ("~>", TokenType::Operator),
            (">>", TokenType::Operator),
            ("..=", TokenType::Operator),
            ("..", TokenType::Operator),
            ("??", TokenType::Operator),
            (".?", TokenType::Operator),
            ("<=>", TokenType::Operator),
            ("<=", TokenType::Operator),
            (">=", TokenType::Operator),
            ("==", TokenType::Operator),
            ("!=", TokenType::Operator),
            ("&&", TokenType::Operator),
            ("||", TokenType::Operator),
            ("+=", TokenType::Operator),
            ("-=", TokenType::Operator),
            ("*=", TokenType::Operator),
            ("/=", TokenType::Operator),
            ("%=", TokenType::Operator),
            // Single-character operators
            ("+", TokenType::Operator),
            ("-", TokenType::Operator),
            ("*", TokenType::Operator),
            ("/", TokenType::Operator),
            ("%", TokenType::Operator),
            ("^", TokenType::Operator),
            ("<", TokenType::Operator),
            (">", TokenType::Operator),
            ("!", TokenType::Operator),
            ("=", TokenType::Operator),
            ("?", TokenType::Operator),
            ("~", TokenType::Operator),
            ("@", TokenType::Operator),
            ("&", TokenType::Operator),
            ("\\", TokenType::LambdaArrow),
        ];

        for (op, token_type) in operators.iter() {
            self.operators.insert(op.to_string(), token_type.clone());
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();

            if self.is_at_end() {
                break;
            }

            let start_line = self.line;
            let start_column = self.column;

            match self.scan_token() {
                Some(token) => tokens.push(token),
                None => {
                    // Skip unknown character and create error token
                    let ch = self.advance();
                    tokens.push(Token::new(
                        TokenType::Unknown,
                        ch.to_string(),
                        start_line,
                        start_column,
                    ));
                }
            }
        }

        tokens.push(Token::new(
            TokenType::EOF,
            "".to_string(),
            self.line,
            self.column,
        ));

        tokens
    }

    fn scan_token(&mut self) -> Option<Token> {
        let start_line = self.line;
        let start_column = self.column;

        // Handle comments first
        if self.peek() == '/' && self.peek_next() == '/' {
            return Some(self.line_comment());
        }

        if self.peek() == '/' && self.peek_next() == '*' {
            return Some(self.block_comment());
        }

        // Handle doc comments
        if self.peek() == '/' && self.peek_next() == '/' && self.peek_ahead(2) == '/' {
            return Some(self.doc_comment());
        }

        // Handle string literals
        if self.peek() == '"' {
            return Some(self.string_literal());
        }

        if self.peek() == '\'' {
            return Some(self.string_literal());
        }

        // Handle multiline strings
        if self.peek() == '"' && self.peek_next() == '"' && self.peek_ahead(2) == '"' {
            return Some(self.multiline_string());
        }

        // Handle interpolated strings
        if self.peek() == '$' && self.peek_next() == '"' {
            return Some(self.interpolated_string());
        }

        // Handle numbers
        if self.peek().is_ascii_digit() {
            return Some(self.number());
        }

        // Handle identifiers and keywords
        if self.peek().is_alphabetic() || self.peek() == '_' {
            return Some(self.identifier());
        }

        // Handle effect markers
        if self.peek() == '!' && (self.peek_next().is_alphabetic() || self.peek_next() == '_') {
            return Some(self.effect_marker());
        }

        // Handle attributes: only when '@' is followed by a lowercase letter.
        // Otherwise treat '@' as a standalone Operator token and let the next Ident be tokenized separately.
        if self.peek() == '@' {
            if self.peek_next().is_ascii_lowercase() {
                return Some(self.attribute());
            } else {
                // consume '@' and return it as an Operator token
                let ch = self.advance();
                return Some(Token::new(
                    TokenType::Operator,
                    ch.to_string(),
                    start_line,
                    start_column,
                ));
            }
        }

        // Handle operators (check multi-character first)
        if let Some(token) = self.operator() {
            return Some(token);
        }

        // Handle single-character tokens
        let ch = self.advance();
        let token_type = match ch {
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '[' => TokenType::LeftBracket,
            ']' => TokenType::RightBracket,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            ';' => TokenType::Semicolon,
            ':' => TokenType::Colon,
            '.' => TokenType::Dot,
            '#' => TokenType::Hash,
            '$' => TokenType::Dollar,
            '`' => TokenType::Backtick,
            '|' => TokenType::Pipe,
            _ => return None,
        };

        Some(Token::new(
            token_type,
            ch.to_string(),
            start_line,
            start_column,
        ))
    }

    fn line_comment(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        while !self.is_at_end() && self.peek() != '\n' {
            lexeme.push(self.advance());
        }

        Token::new(TokenType::LineComment, lexeme, start_line, start_column)
    }

    fn block_comment(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        self.advance(); // consume '/'
        self.advance(); // consume '*'
        lexeme.push_str("/*");

        while !self.is_at_end() {
            if self.peek() == '*' && self.peek_next() == '/' {
                lexeme.push(self.advance()); // consume '*'
                lexeme.push(self.advance()); // consume '/'
                break;
            }
            lexeme.push(self.advance());
        }

        if self.is_at_end() && !lexeme.ends_with("*/") {
            return Token::new(
                TokenType::UnterminatedComment,
                lexeme,
                start_line,
                start_column,
            );
        }

        Token::new(TokenType::BlockComment, lexeme, start_line, start_column)
    }

    fn doc_comment(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        while !self.is_at_end() && self.peek() != '\n' {
            lexeme.push(self.advance());
        }

        Token::new(TokenType::DocComment, lexeme, start_line, start_column)
    }

    fn string_literal(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let quote_char = self.advance(); // consume opening quote
        let mut lexeme = String::new();
        lexeme.push(quote_char);

        while !self.is_at_end() && self.peek() != quote_char {
            if self.peek() == '\\' {
                lexeme.push(self.advance()); // consume '\'
                if !self.is_at_end() {
                    lexeme.push(self.advance()); // consume escaped character
                }
            } else {
                lexeme.push(self.advance());
            }
        }

        if self.is_at_end() {
            return Token::new(
                TokenType::UnterminatedString,
                lexeme,
                start_line,
                start_column,
            );
        }

        lexeme.push(self.advance()); // consume closing quote
        Token::new(TokenType::StringLiteral, lexeme, start_line, start_column)
    }

    fn multiline_string(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        // Consume opening """
        lexeme.push(self.advance());
        lexeme.push(self.advance());
        lexeme.push(self.advance());

        while !self.is_at_end() {
            if self.peek() == '"' && self.peek_next() == '"' && self.peek_ahead(2) == '"' {
                lexeme.push(self.advance());
                lexeme.push(self.advance());
                lexeme.push(self.advance());
                break;
            }
            lexeme.push(self.advance());
        }

        Token::new(
            TokenType::MultilineStringLiteral,
            lexeme,
            start_line,
            start_column,
        )
    }

    fn interpolated_string(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        lexeme.push(self.advance()); // consume '$'
        lexeme.push(self.advance()); // consume '"'

        while !self.is_at_end() && self.peek() != '"' {
            lexeme.push(self.advance());
        }

        if !self.is_at_end() {
            lexeme.push(self.advance()); // consume closing '"'
        }

        Token::new(
            TokenType::InterpolatedStringLiteral,
            lexeme,
            start_line,
            start_column,
        )
    }

    fn number(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();
        let mut is_float = false;

        // Handle hex, binary, octal
        if self.peek() == '0' {
            lexeme.push(self.advance());

            match self.peek() {
                'x' | 'X' => {
                    lexeme.push(self.advance());
                    while self.peek().is_ascii_hexdigit() {
                        lexeme.push(self.advance());
                    }
                }
                'b' | 'B' => {
                    lexeme.push(self.advance());
                    while self.peek() == '0' || self.peek() == '1' {
                        lexeme.push(self.advance());
                    }
                }
                'o' | 'O' => {
                    lexeme.push(self.advance());
                    while self.peek().is_digit(8) {
                        lexeme.push(self.advance());
                    }
                }
                _ => {
                    while self.peek().is_ascii_digit() {
                        lexeme.push(self.advance());
                    }
                }
            }
        } else {
            while self.peek().is_ascii_digit() {
                lexeme.push(self.advance());
            }
        }

        // Handle decimal point
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_float = true;
            lexeme.push(self.advance()); // consume '.'
            while self.peek().is_ascii_digit() {
                lexeme.push(self.advance());
            }
        }

        // Handle scientific notation
        if self.peek() == 'e' || self.peek() == 'E' {
            is_float = true;
            lexeme.push(self.advance());
            if self.peek() == '+' || self.peek() == '-' {
                lexeme.push(self.advance());
            }
            while self.peek().is_ascii_digit() {
                lexeme.push(self.advance());
            }
        }

        // Check for unit literal
        if self.peek().is_alphabetic() {
            let mut unit = String::new();
            while self.peek().is_alphabetic() {
                unit.push(self.advance());
            }

            // Common units
            let valid_units = ["km", "m", "cm", "mm", "h", "min", "s", "ms", "deg", "rad"];
            if valid_units.contains(&unit.as_str()) {
                lexeme.push_str(&unit);
                return Token::new(TokenType::UnitLiteral, lexeme, start_line, start_column);
            } else {
                // Invalid unit
                lexeme.push_str(&unit);
                return Token::new(TokenType::InvalidUnit, lexeme, start_line, start_column);
            }
        }

        let token_type = if is_float {
            TokenType::FloatLiteral
        } else {
            TokenType::IntLiteral
        };

        Token::new(token_type, lexeme, start_line, start_column)
    }

    fn identifier(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        while self.peek().is_alphanumeric() || self.peek() == '_' {
            lexeme.push(self.advance());
        }

        // Check for module path
        if self.peek() == ':' && self.peek_next() == ':' {
            lexeme.push_str("::");
            self.advance();
            self.advance();

            // Continue reading the rest of the path
            while self.peek().is_alphanumeric() || self.peek() == '_' {
                lexeme.push(self.advance());

                if self.peek() == ':' && self.peek_next() == ':' {
                    lexeme.push_str("::");
                    self.advance();
                    self.advance();
                }
            }

            return Token::new(TokenType::ModulePath, lexeme, start_line, start_column);
        }

        // Check if it's a keyword
        let token_type = self
            .keywords
            .get(&lexeme)
            .cloned()
            .unwrap_or(TokenType::Identifier);

        Token::new(token_type, lexeme, start_line, start_column)
    }

    fn effect_marker(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        lexeme.push(self.advance()); // consume '!'

        while self.peek().is_alphanumeric() || self.peek() == '_' {
            lexeme.push(self.advance());
        }

        Token::new(TokenType::EffectMarker, lexeme, start_line, start_column)
    }

    fn attribute(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut lexeme = String::new();

        lexeme.push(self.advance()); // consume '@'

        while self.peek().is_alphanumeric() || self.peek() == '_' {
            lexeme.push(self.advance());
        }

        // Check for parameterized attribute
        if self.peek() == '(' {
            let mut paren_count = 0;
            while !self.is_at_end() {
                let ch = self.advance();
                lexeme.push(ch);

                match ch {
                    '(' => paren_count += 1,
                    ')' => {
                        paren_count -= 1;
                        if paren_count == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
            }
            return Token::new(
                TokenType::ParameterizedAttribute,
                lexeme,
                start_line,
                start_column,
            );
        }

        Token::new(TokenType::Attribute, lexeme, start_line, start_column)
    }

    fn operator(&mut self) -> Option<Token> {
        let start_line = self.line;
        let start_column = self.column;

        // Try multi-character operators first (longest first)
        let mut candidates = Vec::new();

        for (op, _) in &self.operators {
            if self.matches_string(op) {
                candidates.push(op.clone());
            }
        }

        // Sort by length (descending) to match longest first
        candidates.sort_by(|a, b| b.len().cmp(&a.len()));

        if let Some(op) = candidates.first() {
            let token_type = self.operators[op].clone();

            // Consume the operator characters
            for _ in 0..op.len() {
                self.advance();
            }

            return Some(Token::new(token_type, op.clone(), start_line, start_column));
        }

        None
    }

    fn matches_string(&self, s: &str) -> bool {
        let chars: Vec<char> = s.chars().collect();

        if self.current + chars.len() > self.input.len() {
            return false;
        }

        for (i, &ch) in chars.iter().enumerate() {
            if self.input[self.current + i] != ch {
                return false;
            }
        }

        true
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\r' | '\t' | '\n' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.input.len()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.input.len() {
            '\0'
        } else {
            self.input[self.current + 1]
        }
    }

    fn peek_ahead(&self, offset: usize) -> char {
        if self.current + offset >= self.input.len() {
            '\0'
        } else {
            self.input[self.current + offset]
        }
    }

    fn advance(&mut self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            let ch = self.input[self.current];
            self.current += 1;
            self.column += 1;
            ch
        }
    }
}
