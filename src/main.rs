pub mod interpreter;
use std::fs::File;
use std::io::Read;

fn get_file_contents(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}

fn main() -> std::io::Result<()> {
    let string = get_file_contents("./working_files/test.vgr")?;
    let result = interpreter::lexer::Lexer::lex(string);
    match result {
        Ok(tokens) => {
            let mut formatted = format!("{:?}", tokens);
            formatted = formatted
                .replace(",", ",\n   ")
                .replace("[", "[\n    ")
                .replace("]", "\n]")
                .replace("\n    char_range", " char_range");
            println!("{}", formatted);
        }
        Err(err) => println!("{}", err),
    }
    Ok(())
}