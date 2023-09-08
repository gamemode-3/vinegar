use std::{rc::Rc, slice::Iter};

struct A {
    b: EnumB,
}

enum EnumA {
    A(Rc<A>),
}

struct B {
    a: EnumA,
}

enum EnumB {
    B(B),
    C(C),
}

struct C {}

fn main() {
    EnumA::A(A { b: EnumB::C(C {}) });
}
