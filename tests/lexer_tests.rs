#[cfg(test)]
mod tests {
    use vinegar::debug;
    use vinegar::interpreter::lexer::*;
    #[test]
    fn test_on_file() {
        let output = match Lexer::lex_file("./working_files/test.vgr".to_string()) {
            Ok(v) => debug::format_token_vec_simplified(&v),
            Err(e) => format!("{}", e),
        };

        println!("{}", output)
    }
}
