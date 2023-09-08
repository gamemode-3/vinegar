#[cfg(test)]
mod tests {
    use vinegar::interpreter::parser::*;

    #[test]
    fn test_on_file() {
        let path = "./working_files/test.vgr";

        let output: String = match Parser::parse_file(path.into()) {
            Ok(c) => format!("{:?}", c),
            Err(e) => format!("{}", e),
        };

        println!("{}", output)
    }

    fn test_assignment() {
        // assert_eq!(
        //     Parser::parse_string("name = value".into()),
        //     Ok(CodeBody { statements: vec![Statement::] })
        // )
    }
}
