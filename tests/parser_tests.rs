#[cfg(test)]
mod tests {
    use vinegar::debug;
    use vinegar::interpreter::parser::*;
    #[test]
    fn test_on_file() {
        let path = "./working_files/test.vgr";

        let msl = 60;
        let output: String = match Parser::parse_file(path.into()) {
            Ok(c) => debug::format_parens(&format!("{:?}", c), msl),
            Err(e) => format!("{}", e),
        };

        println!("{}", output)
    }
}
