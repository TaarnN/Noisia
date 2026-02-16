#[path = "../src/style.rs"]
mod style;
#[path = "../src/tokenizer.rs"]
mod tokenizer;
#[path = "../src/ptypes.rs"]
mod ptypes;
#[path = "../src/parser.rs"]
mod parser;

use std::fs;
use std::path::PathBuf;

fn fixture_path(relative: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(relative)
}

fn parse_program_from_fixture(relative: &str) -> Result<ptypes::Program, ptypes::ParseError> {
    let path = fixture_path(relative);
    let src = fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read fixture {}: {}", path.display(), err));

    let mut tokenizer = tokenizer::Tokenizer::new(&src);
    let tokens = tokenizer.tokenize();

    let mut parser = parser::Parser::new(tokens);
    parser.parse_program()
}

#[test]
fn parse_success_fixtures_from_plan() {
    let fixtures = [
        "tests/parser/6-control-flow.nx",
        "tests/parser/7-range-slice-optchain.nx",
        "tests/parser/8-macro-igm-extension.nx",
        "tests/parser/9-context-using-constexpr.nx",
        "tests/parser/10-pointer-ops.nx",
        "tests/parser/11-class-advanced.nx",
        "tests/parser/12-temporal-40-45.nx",
    ];

    for fixture in fixtures {
        if let Err(err) = parse_program_from_fixture(fixture) {
            panic!("expected parse success for {}\n{:#?}", fixture, err);
        }
    }
}

#[test]
fn temporal_fixture_contains_new_temporal_nodes() {
    let program = parse_program_from_fixture("tests/parser/12-temporal-40-45.nx")
        .expect("temporal fixture should parse");
    let dump = format!("{:#?}", program);

    assert!(dump.contains("Rewind"), "expected rewind nodes in AST");
    assert!(
        dump.contains("else_expr: Some"),
        "expected rewind expression fallback in AST"
    );
    assert!(
        dump.contains("TemporalScope") && dump.contains("config: Some"),
        "expected temporal scope config block in AST"
    );
    assert!(
        dump.contains("TemporalTransaction") && dump.contains("catch_name: Some"),
        "expected temporal transaction with catch in AST"
    );
    assert!(
        dump.contains("TemporalTest") && dump.contains("TemporalMemory"),
        "expected temporal test and memory nodes in AST"
    );
    assert!(
        dump.contains("DebugTemporal") && dump.contains("Raw("),
        "expected debug temporal raw clauses in AST"
    );
    assert!(
        dump.contains("TemporalMatch") && dump.contains("pattern: Raw("),
        "expected match temporal state raw patterns in AST"
    );
}

#[test]
fn parse_failure_fixtures() {
    let fixtures = [
        "tests/parser/fail/01-unterminated-regex.nx",
        "tests/parser/fail/02-rewind-expression-missing-else.nx",
        "tests/parser/fail/03-temporal-match-missing-arrow.nx",
        "tests/parser/fail/04-checkpoint-double-name.nx",
    ];

    for fixture in fixtures {
        let result = parse_program_from_fixture(fixture);
        assert!(result.is_err(), "expected parse failure for {}", fixture);
    }
}
