mod parser;
mod tokenizer;

fn main() {
    let code = "
module App::Hello

@entry
fn main() -> Int {
    println((\"Hello, Noisia!\") + (1+2).toString().ext(11,2))
    return 0
}";

    let mut tokenizer = tokenizer::Tokenizer::new(code);
    let tokens = tokenizer.tokenize();

    println!("Tokenization Results:");
    println!("===================");

    for token in &tokens {
        println!("{}", token)
    }

    let mut parser = parser::Parser::new(tokens);
    let items = parser.parse_program().unwrap().items;

    println!("\n\nParse Results:");
    println!("===================");

    for item in &items {
        println!("{}", item);
    }
}
