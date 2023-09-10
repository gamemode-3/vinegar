use std::fs::File;
use std::io::Read;

pub fn get_file_contents(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}
