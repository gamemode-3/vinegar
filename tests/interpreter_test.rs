#[cfg(test)]
mod tests {
    use vinegar::debug;
    use vinegar::interpreter::interpreter::Interpreter;

    #[test]
    fn test_on_file() {
        let path = "./working_files/test.vgr";
        
        println!("\n\n");

        match Interpreter::interpret_file(path.into()) {
            Ok(_) => (),
            Err(e) => println!("{}", e),
        };
        
        println!("\n\n");
    }
}
