mod parser;
mod ptypes;
mod style;
mod tokenizer;

use std::fs;
use std::env;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let out_style = style::Style::auto_stdout();
    let err_style = style::Style::auto_stderr();

    let path = env::args().nth(1).unwrap_or_else(|| "./main.nx".to_string());
    let code = fs::read_to_string(&path)?;

    let mut tokenizer = tokenizer::Tokenizer::new(&code);
    let tokens = tokenizer.tokenize();

    println!(
        "{} {}",
        out_style.bold("Input file:"),
        out_style.underline(&path)
    );
    println!(
        "{} ({})",
        out_style.paint("Tokens", &["1", "36"]),
        out_style.fg_magenta(&tokens.len().to_string())
    );
    println!("{}", out_style.dim("----------------------------------------"));

    for token in &tokens {
        println!("  {}", token.pretty(&out_style))
    }

    let mut parser = parser::Parser::new(tokens);
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => {
            eprintln!("\n{}", err.pretty(&err_style));
            std::process::exit(1);
        }
    };
    let items = program.items;

    println!(
        "\n{} (items: {})",
        out_style.paint("AST", &["1", "36"]),
        out_style.fg_magenta(&items.len().to_string())
    );
    println!("{}", out_style.dim("----------------------------------------"));

    for item in &items {
        println!("{}", item.pretty(&out_style));
    }

    Ok(())
}
