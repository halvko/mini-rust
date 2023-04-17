use parse;

fn main() {
    parse::gen_ast(
        "fn main() {}
         fn foo() {}
         fn bar() {}",
    );
}
