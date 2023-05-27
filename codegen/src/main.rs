use std::io::stdout;

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
    let parsed = parse::gen_ast(input).unwrap();
    let tted = typecheck::type_check(parsed);
    let mut stdout = stdout();
    codegen::gen_ir(tted, &mut stdout, Default::default()).unwrap();
}
