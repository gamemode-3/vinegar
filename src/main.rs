mod interpreter;

fn main() {
    let result = format!("{:?}", interpreter::lexer::Lexer::new("Hi ").get_tokens());
    println!("{}", result);
}
