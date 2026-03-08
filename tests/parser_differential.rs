#![allow(dead_code)]

use std::fs;
use std::path::{Path, PathBuf};

use Noisia::parser::Parser as CurrentParser;
use Noisia::ptypes::ParseError as CurrentParseError;
use Noisia::tokenizer::Tokenizer as CurrentTokenizer;

#[path = "../backup/ptypes.rs"]
mod ptypes;
#[path = "../backup/style.rs"]
mod style;
#[path = "../backup/tokenizer.rs"]
mod tokenizer;
#[path = "../backup/parser.rs"]
mod parser;

use parser::Parser as OracleParser;
use ptypes::ParseError as OracleParseError;
use tokenizer::Tokenizer as OracleTokenizer;

fn fixture_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("parser")
}

fn sorted_fixture_paths() -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = fs::read_dir(fixture_dir())
        .expect("fixtures dir should exist")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|v| v.to_str()) == Some("nx"))
        .collect();
    files.sort();
    files
}

fn normalize_current_error(err: &CurrentParseError) -> String {
    match err {
        CurrentParseError::UnexpectedToken {
            expected,
            found,
            idx,
        } => format!(
            "UnexpectedToken|expected={}|found={:?}|lexeme={}|line={}|col={}|idx={}",
            expected, found.token_type, found.lexeme, found.line, found.column, idx
        ),
        CurrentParseError::EOF {
            message,
            line,
            column,
        } => format!("EOF|message={}|line={}|col={}", message, line, column),
        CurrentParseError::Generic {
            message,
            line,
            column,
        } => format!("Generic|message={}|line={}|col={}", message, line, column),
    }
}

fn normalize_oracle_error(err: &OracleParseError) -> String {
    match err {
        OracleParseError::UnexpectedToken {
            expected,
            found,
            idx,
        } => format!(
            "UnexpectedToken|expected={}|found={:?}|lexeme={}|line={}|col={}|idx={}",
            expected, found.token_type, found.lexeme, found.line, found.column, idx
        ),
        OracleParseError::EOF {
            message,
            line,
            column,
        } => format!("EOF|message={}|line={}|col={}", message, line, column),
        OracleParseError::Generic {
            message,
            line,
            column,
        } => format!("Generic|message={}|line={}|col={}", message, line, column),
    }
}

#[test]
fn parser_matches_backup_oracle_on_fixture_corpus() {
    let fixtures = sorted_fixture_paths();
    assert!(!fixtures.is_empty(), "fixture corpus should not be empty");

    for fixture in fixtures {
        let source = fs::read_to_string(&fixture).expect("fixture should be readable");

        let mut current_tokenizer = CurrentTokenizer::new(&source);
        let current_tokens = current_tokenizer.tokenize();
        let mut current_parser = CurrentParser::new(current_tokens);
        let current_result = current_parser.parse_program();

        let mut oracle_tokenizer = OracleTokenizer::new(&source);
        let oracle_tokens = oracle_tokenizer.tokenize();
        let mut oracle_parser = OracleParser::new(oracle_tokens);
        let oracle_result = oracle_parser.parse_program();

        let fixture_name = fixture
            .file_name()
            .and_then(|v| v.to_str())
            .unwrap_or("<unknown>");

        match (current_result, oracle_result) {
            (Ok(current_program), Ok(oracle_program)) => {
                let current_dump = format!("{:#?}", current_program);
                let oracle_dump = format!("{:#?}", oracle_program);
                assert_eq!(
                    current_dump, oracle_dump,
                    "AST mismatch for fixture: {}",
                    fixture_name
                );
            }
            (Err(current_err), Err(oracle_err)) => {
                let current_sig = normalize_current_error(&current_err);
                let oracle_sig = normalize_oracle_error(&oracle_err);
                assert_eq!(
                    current_sig, oracle_sig,
                    "ParseError mismatch for fixture: {}",
                    fixture_name
                );
            }
            (Ok(current_program), Err(oracle_err)) => {
                panic!(
                    "current parsed but oracle failed for fixture {}\ncurrent:\n{:#?}\noracle:\n{}",
                    fixture_name,
                    current_program,
                    normalize_oracle_error(&oracle_err)
                );
            }
            (Err(current_err), Ok(oracle_program)) => {
                panic!(
                    "current failed but oracle parsed for fixture {}\ncurrent:\n{}\noracle:\n{:#?}",
                    fixture_name,
                    normalize_current_error(&current_err),
                    oracle_program
                );
            }
        }
    }
}
