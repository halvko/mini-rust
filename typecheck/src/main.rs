use typecheck::type_check;

fn main() {
    let input = "fn main() {
    let a = 42;
    let b = 69;
    let c = loop {
        if a == b {
            break true;
        } else {
            print(a);
            a = (a + 1);
        }
    };
}";
    println!("{input}");
    let parsed = parse::gen_ast(input).unwrap();
    let tc = type_check(parsed);
    println!("{tc}")
}
