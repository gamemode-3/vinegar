#[cfg(test)]
mod tests {
    use vinegar::interpreter::runtime::{try_from_vinegar_object, VinegarRuntime, VinegarObject};
    use vinegar::interpreter::vinegar_std::Duck;

    #[test]
    fn test_on_file() {
        let path = "./working_files/test.vgr";

        println!("\n\n");

        let obj = match VinegarRuntime::interpret_file(path.into()) {
            Ok(v) => v,
            Err(e) => return println!("{}", e),
        };

        let a: Duck = match try_from_vinegar_object(obj) {
            Some(v) => v,
            None => return println!("Could not convert object to struct"),
        };

        println!("{:?}", a);

        println!("\n\n");
    }
}
