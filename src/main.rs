mod tokenizer;
mod parser;

fn main() {
    let code = "
fn greet() -> Void !Network {}";

    let mut tokenizer = tokenizer::Tokenizer::new(code);
    let tokens = tokenizer.tokenize();

    println!("Tokenization Results:");
    println!("===================");

    for token in tokens {
        println!("{}", token)
    }
}
