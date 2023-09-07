use std::slice::Iter;

struct MyStruct {
    vector: Vec<usize>,
    vector_iter: Iter<'static, Vec<usize>>,
}

fn main() {
    let a: Vec<usize> = vec![];
    a.into_iter();
}
