#[cfg(test)]
mod tests {
    use vinegar::interpreter::runtime::VinegarRuntime;

    #[test]
    fn test_on_file() {
        let path = "./working_files/test.vgr";

        println!("\n\n");

        match VinegarRuntime::interpret_file(path.into()) {
            Ok(_) => (),
            Err(e) => println!("{}", e),
        };

        println!("\n\n");
    }
}
