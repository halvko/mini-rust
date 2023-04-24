fn main() {
    let mut i = 0;
    let a = loop {
        if i >= 10 {
            break 42;
        };
        i = (i + 1)
    };
}
