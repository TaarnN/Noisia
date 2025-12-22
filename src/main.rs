mod parser;
mod tokenizer;
mod ptypes;

fn main() {
    let code = "
enum Shape {
    Circle { radius: Float },
    Rect { width: Float, height: Float },
    Polygon(Vector<Point>),
}
";

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
