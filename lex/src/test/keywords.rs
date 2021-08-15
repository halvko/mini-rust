use super::prelude::*;

#[test]
fn lex_fn() {
    let pos = &mut Pos::new();
    lex_test("fn", &[(Token::KeyWord(KeyWord::Fn), pos.update(2))]);
}

#[test]
fn lex_let() {
    let pos = &mut Pos::new();
    lex_test("let", &[(Token::KeyWord(KeyWord::Let), pos.update(3))]);
}
