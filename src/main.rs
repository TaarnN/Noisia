mod parser;
mod tokenizer;

fn main() {
    let code = "
@entry
fn main(a: ~Int) {
    
    if 1 < 2 {
        let~ a: Int? = 5
        println(a)
    }
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
