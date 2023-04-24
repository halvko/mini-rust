fn main() {
    println!(
        "{:#?}",
        parse::gen_ast(
            "fn main() {}
         fn foo() {}
         fn bar() {}",
        )
        .unwrap()
    )
}
