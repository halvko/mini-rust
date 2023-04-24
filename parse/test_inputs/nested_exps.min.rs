fn main() {
    let a = 42;
    let b = 69;
    if (a + b) < maths(a, b) {
        return 42;
    }
}

fn maths(a: i32, b: i32) -> i32 {
    let mut c = a * b;
    c = (c + 1);
    c
}
