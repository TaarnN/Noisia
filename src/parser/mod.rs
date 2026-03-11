#![allow(unused_variables)]
#![allow(unused_assignments)]
#![allow(dead_code)]
#![allow(unused_mut)]

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
            Item::Using(inner) => write!(f, "{:#?}", inner),
            Item::AttributeConfig(inner) => write!(f, "{:#?}", inner),
            Item::MacroDecl(inner) => write!(f, "{:#?}", inner),
            Item::ExtensionDecl(inner) => write!(f, "{:#?}", inner),
            Item::IGMDecl(inner) => write!(f, "{:#?}", inner),
            Item::PluginDecl(inner) => write!(f, "{:#?}", inner),
            Item::Function(inner) => write!(f, "{:#?}", inner),
            Item::Struct(inner) => write!(f, "{:#?}", inner),
            Item::Enum(inner) => write!(f, "{:#?}", inner),
            Item::Trait(inner) => write!(f, "{:#?}", inner),
            Item::Impl(inner) => write!(f, "{:#?}", inner),
            Item::MixinDecl(inner) => write!(f, "{:#?}", inner),
            Item::InterfaceDecl(inner) => write!(f, "{:#?}", inner),
            Item::ProtocolDecl(inner) => write!(f, "{:#?}", inner),
            Item::Class(inner) => write!(f, "{:#?}", inner),
        }
    }
}

impl Item {
    pub fn pretty(&self, style: &Style) -> String {
        let raw = match self {
            Item::ModuleDecl(inner) => format!("{:#?}", inner),
            Item::Import(inner) => format!("{:#?}", inner),
            Item::Using(inner) => format!("{:#?}", inner),
            Item::AttributeConfig(inner) => format!("{:#?}", inner),
            Item::MacroDecl(inner) => format!("{:#?}", inner),
            Item::ExtensionDecl(inner) => format!("{:#?}", inner),
            Item::IGMDecl(inner) => format!("{:#?}", inner),
            Item::PluginDecl(inner) => format!("{:#?}", inner),
            Item::Function(inner) => format!("{:#?}", inner),
            Item::Struct(inner) => format!("{:#?}", inner),
            Item::Enum(inner) => format!("{:#?}", inner),
            Item::Trait(inner) => format!("{:#?}", inner),
            Item::Impl(inner) => format!("{:#?}", inner),
            Item::MixinDecl(inner) => format!("{:#?}", inner),
            Item::InterfaceDecl(inner) => format!("{:#?}", inner),
            Item::ProtocolDecl(inner) => format!("{:#?}", inner),
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
    matches!(trimmed.chars().next(), Some('}') | Some(']') | Some(')'))
}

fn strip_string_delimiters(lexeme: &str) -> String {
    if lexeme.len() >= 6 && lexeme.starts_with("\"\"\"") && lexeme.ends_with("\"\"\"") {
        return lexeme[3..lexeme.len() - 3].to_string();
    }

    if lexeme.len() >= 2 {
        let first = lexeme.as_bytes()[0] as char;
        let last = lexeme.as_bytes()[lexeme.len() - 1] as char;
        if (first == '"' || first == '\'') && first == last {
            return lexeme[1..lexeme.len() - 1].to_string();
        }
    }

    lexeme.to_string()
}

pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
    allow_struct_literals: bool,
    allow_trailing_closures: bool,
}

enum FieldOrProperty {
    Field(ClassFieldDecl),
    Property(ClassPropertyDecl),
}

mod class;
mod core;
mod expressions;
mod items;
mod patterns;
mod statements;
mod temporal;
mod types;

// note: operator precedence table
pub fn op_precedence(tok: &Token) -> Option<(u8, bool)> {
    match tok.lexeme.as_str() {
        "??" | "||" | "or" => Some((1, false)),
        ".." | "..=" | "&&" | "and" => Some((2, false)),
        "==" | "!=" | "<" | "<=" | ">" | ">=" | "<@" | "@>" => Some((3, false)),
        "|>" | ">>" | "~>" => Some((11, false)),
        "|" => Some((4, false)),
        "^" => Some((5, false)),
        "&" => Some((6, false)),
        "<<" => Some((7, false)),
        "+" | "-" | "+>" | "<+" | "<->" => Some((8, false)),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::Tokenizer;

    fn parse_fn_body(src: &str) -> Block {
        let mut tokenizer = Tokenizer::new(src);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().expect("program should parse");
        assert_eq!(program.items.len(), 1, "expected one top-level item");
        match &program.items[0] {
            Item::Function(func) => func.body.clone().expect("function should have body"),
            other => panic!("expected function item, got {other:?}"),
        }
    }

    #[test]
    fn parses_await_as_unary_expr() {
        let body = parse_fn_body(
            r#"
            fn main() {
                let data = await task
            }
            "#,
        );

        match &body.stmts[0] {
            Stmt::Let {
                expr: Some(Expr::Await(inner)),
                ..
            } => {
                assert!(matches!(inner.as_ref(), Expr::Ident(name) if name == "task"));
            }
            other => panic!("expected let-await statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_ptr_arrow_method_as_single_expression() {
        let body = parse_fn_body(
            r#"
            fn main() {
                let x = file->read()
            }
            "#,
        );

        assert_eq!(
            body.stmts.len(),
            1,
            "ptr->method() should remain one statement"
        );
        match &body.stmts[0] {
            Stmt::Let {
                expr:
                    Some(Expr::MethodCall {
                        object,
                        method,
                        args,
                        ..
                    }),
                ..
            } => {
                assert_eq!(method, "read");
                assert!(args.is_empty());
                assert!(matches!(
                    object.as_ref(),
                    Expr::PointerDeref { expr, safe: false }
                    if matches!(expr.as_ref(), Expr::Ident(name) if name == "file")
                ));
            }
            other => panic!("expected let with pointer method call, got {other:?}"),
        }
    }

    #[test]
    fn parses_pointer_pipeline_ptr_arrow_transform_star() {
        let body = parse_fn_body(
            r#"
            fn main() {
                let result = data |> ptr-> transform(*)
            }
            "#,
        );

        match &body.stmts[0] {
            Stmt::Let {
                expr: Some(Expr::Pipeline { left, right }),
                ..
            } => {
                assert!(matches!(left.as_ref(), Expr::Ident(name) if name == "data"));
                match right.as_ref() {
                    Expr::MethodCall {
                        object,
                        method,
                        args,
                        ..
                    } => {
                        assert_eq!(method, "transform");
                        assert_eq!(args.len(), 1);
                        assert!(matches!(args[0], Expr::Ident(ref name) if name == "*"));
                        assert!(matches!(
                            object.as_ref(),
                            Expr::PointerDeref { expr, safe: false }
                            if matches!(expr.as_ref(), Expr::Ident(name) if name == "ptr")
                        ));
                    }
                    other => panic!("expected ptr->transform(*) as method call, got {other:?}"),
                }
            }
            other => panic!("expected pipeline let statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_trailing_closure_after_call() {
        let body = parse_fn_body(
            r#"
            fn main() {
                FileManager.withFile("data.txt") { manager in
                    manager.write("Hello")
                }
            }
            "#,
        );

        match &body.stmts[0] {
            Stmt::Expr(Expr::MethodCall { method, args, .. }) => {
                assert_eq!(method, "withFile");
                assert_eq!(
                    args.len(),
                    2,
                    "trailing closure should be appended as final arg"
                );
                match args.last().unwrap() {
                    Expr::Lambda { params, .. } => {
                        assert_eq!(params.len(), 1);
                        assert!(matches!(
                            params[0].pattern.kind,
                            PatternKind::Identifier(ref name) if name == "manager"
                        ));
                    }
                    other => panic!("expected lambda as trailing arg, got {other:?}"),
                }
            }
            other => panic!("expected call expression statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_trailing_closure_field_style_call_without_parens() {
        let body = parse_fn_body(
            r#"
            fn main() {
                let server = WebServer.configure { server in
                    server.route("/api/users", handler)
                }
            }
            "#,
        );

        match &body.stmts[0] {
            Stmt::Let {
                expr: Some(Expr::MethodCall { method, args, .. }),
                ..
            } => {
                assert_eq!(method, "configure");
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expr::Lambda { .. }));
            }
            other => panic!("expected let with trailing-closure method call, got {other:?}"),
        }
    }

    #[test]
    fn parses_match_statement_arm_block_body() {
        let body = parse_fn_body(
            r#"
            fn main() {
                match self.head {
                    Some(node) :> {
                        self.head = node.next->?
                        Some(node.value->)
                    }
                    None :> None
                }
            }
            "#,
        );

        match &body.stmts[0] {
            Stmt::Match { arms, .. } => {
                assert_eq!(arms.len(), 2);
                assert!(matches!(arms[0].body, Expr::Block(_)));
                assert!(matches!(arms[1].body, Expr::Ident(ref name) if name == "None"));
            }
            other => panic!("expected match statement, got {other:?}"),
        }
    }

    #[test]
    fn parses_match_type_with_array_arm_without_greedy_body_capture() {
        let body = parse_fn_body(
            r#"
            fn main() {
                match Type(data) {
                    Int:> processNumber(data)
                    String:> processText(data)
                    [Any]:> processArray(data)
                    _:> processDefault(data)
                }
            }
            "#,
        );

        match &body.stmts[0] {
            Stmt::Match { arms, .. } => {
                assert_eq!(arms.len(), 4);
                assert!(
                    !matches!(arms[1].body, Expr::Index { .. }),
                    "previous arm body should not greedily capture [Any]"
                );
                assert!(matches!(arms[2].pattern.kind, PatternKind::Array(_)));
            }
            other => panic!("expected match statement, got {other:?}"),
        }
    }
}
