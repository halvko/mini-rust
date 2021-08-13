use super::prelude::*;

#[test]
fn int() {
    let pos = &mut Pos::new();
    lex_test("123", &[(Token::Int("123"), pos.update(3))]);
}

#[test]
fn typed_int() {
    let pos = &mut Pos::new();
    lex_test(
        "123u32",
        &[
            (Token::Int("123"), pos.update(3)),
            (Token::Ident("u32"), pos.update(3)),
        ],
    );
}

#[test]
fn int_with_seperator() {
    let pos = &mut Pos::new();
    lex_test("123_000", &[(Token::Int("123_000"), pos.update(7))]);
}
