mod parser;
mod ptypes;
mod tokenizer;

fn main() {
    let code = "
@entry
fn main() {
    let double = \\x :> x * 2;
    let add = \\x, y :> x + y;
    
    let result = [1, 2, 3, 4] |> double |> \\xs :> xs.length;
    
    let pipeline_result = 5 |> \\n :> n * 10 |> \\n :> n + 5;

    let complex = \\x, y, z :> (x + y) * z |> \\n :> n / 2;
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
