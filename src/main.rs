mod debug;
mod file_handler;
mod interpreter;
use interpreter::lexer::Lexer;
use interpreter::parser::Parser;


fn main() {
    let path = "./working_files/test.vgr";

    let output = match Lexer::lex_file(path.into()) {
        Ok(v) => debug::format_token_vec_simplified(&v),
        Err(e) => format!("{}", e),
    };

    println!("{}", output);


    let output = match Parser::parse_file(path.into()) {
        Ok(v) => format!("{:?}", &v),
        Err(e) => format!("{}", e),
    };

    println!("{}", output)
}
