mod tokenizer;
mod parser;

fn main() {
    let code = "
@entry
fn main() {
    let a = 5
}";

    let mut tokenizer = tokenizer::Tokenizer::new(code);
    let tokens = tokenizer.tokenize();

    println!("Tokenization Results:");
    println!("===================");

    for token in tokens {
        println!("{}", token)
    }
}
