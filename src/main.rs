mod parser;
mod ptypes;
mod tokenizer;

use std::fs;
use std::env;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = env::args().nth(1).unwrap_or_else(|| "./main.nx".to_string());
    let code = fs::read_to_string(&path)?;

    let mut tokenizer = tokenizer::Tokenizer::new(&code);
    let tokens = tokenizer.tokenize();

    println!("Input file: {}", path);
    println!("Tokens ({})", tokens.len());
    println!("----------------------------------------");

    for token in &tokens {
        println!("  {}", token)
    }

    let mut parser = parser::Parser::new(tokens);
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => {
            eprintln!("\n{}", err);
            std::process::exit(1);
        }
    };
    let items = program.items;

    println!("\nAST (items: {})", items.len());
    println!("----------------------------------------");

    for item in &items {
        println!("{}", item);
    }

    Ok(())
}
