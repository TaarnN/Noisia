mod interpreter;
mod parser;
mod ptypes;
mod style;
mod tokenizer;

use std::env;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let out_style = style::Style::auto_stdout();
    let err_style = style::Style::auto_stderr();

    let mut path = "./main.nx".to_string();
    let mut run_interpreter = false;

    for arg in env::args().skip(1) {
        if arg == "--run" {
            run_interpreter = true;
        } else {
            path = arg;
        }
    }

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
    println!(
        "{}",
        out_style.dim("----------------------------------------")
    );

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
    let items = &program.items;

    println!(
        "\n{} (items: {})",
        out_style.paint("AST", &["1", "36"]),
        out_style.fg_magenta(&items.len().to_string())
    );
    println!(
        "{}",
        out_style.dim("----------------------------------------")
    );

    for item in items {
        println!("{}", item.pretty(&out_style));
    }

    if run_interpreter {
        println!("\n{}", out_style.paint("Interpreted Result", &["1", "36"]),);
        println!(
            "{}",
            out_style.dim("----------------------------------------")
        );
        #[cfg(feature = "inkwell-jit")]
        let mut interpreter =
            interpreter::Interpreter::with_backend(interpreter::InkwellJitBackend::default());
        #[cfg(not(feature = "inkwell-jit"))]
        let mut interpreter = interpreter::Interpreter::new();

        match interpreter.run_program(&program) {
            Ok(value) => {
                println!(
                    "\n{} {}",
                    out_style.paint("Interpreter result:", &["1", "36"]),
                    out_style.fg_magenta(&value.to_string())
                );
            }
            Err(err) => {
                eprintln!(
                    "\n{} {}",
                    err_style.paint("error[interpreter]", &["1", "31"]),
                    err_style.fg_cyan(&err.to_string())
                );
                std::process::exit(1);
            }
        }
    }

    Ok(())
}
