mod parser;
mod tokenizer;

fn main() {
    let code = "
// Class พื้นฐานพร้อม constructor
class Person {
    private let name: String
    let age: Int // = public age: Int

    // Primary constructor
    fn init(name: String, age: Int) {
        self.name = name
        self.age = age
    }

    // Secondary constructor
    fn initwithName(name: String) {
        self.init(name, 0)
    }
}

@entry
fn main() {
   // การสร้าง instance หลายแบบ
   let p1 = Person(\"Alice\", 25)           // Primary constructor
   let p2 = Person.withName(\"Bob\")        // Named constructor
   let p3 = Person { name: \"Carol\", age: 30 }  // Struct-like syntax
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
