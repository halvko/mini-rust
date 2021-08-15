use super::prelude::*;

#[test]
fn fn_def() {
    let pos = &mut Pos::new();
    lex_test(
        "fn lul(){}",
        &[
            (KeyWord(KeyWord::Fn), pos.update(2)),
            (WhiteSpace, pos.update(1)),
            (Ident("lul"), pos.update(3)),
            (Bracket(Bracket::Start), pos.update(1)),
            (Bracket(Bracket::End), pos.update(1)),
            (Curley(Bracket::Start), pos.update(1)),
            (Curley(Bracket::End), pos.update(1)),
        ],
    );
}

#[test]
fn fn_with_args() {
    let pos = &mut Pos::new();
    lex_test(
        "fn lul(a: u16, b: String){}",
        &[
            (KeyWord(KeyWord::Fn), pos.update(2)),
            (WhiteSpace, pos.update(1)),
            (Ident("lul"), pos.update(3)),
            (Bracket(Bracket::Start), pos.update(1)),
            (Ident("a"), pos.update(1)),
            (Colon, pos.update(1)),
            (WhiteSpace, pos.update(1)),
            (Ident("u16"), pos.update(3)),
            (Comma, pos.update(1)),
            (WhiteSpace, pos.update(1)),
            (Ident("b"), pos.update(1)),
            (Colon, pos.update(1)),
            (WhiteSpace, pos.update(1)),
            (Ident("String"), pos.update(6)),
            (Bracket(Bracket::End), pos.update(1)),
            (Curley(Bracket::Start), pos.update(1)),
            (Curley(Bracket::End), pos.update(1)),
        ],
    );
}

#[test]
fn fn_with_return() {
    let pos = &mut Pos::new();
    lex_test(
        "fn argh() -> u16 {}",
        &[
            (KeyWord(KeyWord::Fn), pos.update(2)),
            (WhiteSpace, pos.update(1)),
            (Ident("argh"), pos.update(4)),
            (Bracket(Bracket::Start), pos.update(1)),
            (Bracket(Bracket::End), pos.update(1)),
            (WhiteSpace, pos.update(1)),
            (Arrow, pos.update(2)),
            (WhiteSpace, pos.update(1)),
            (Ident("u16"), pos.update(3)),
            (WhiteSpace, pos.update(1)),
            (Curley(Bracket::Start), pos.update(1)),
            (Curley(Bracket::End), pos.update(1)),
        ],
    );
}

#[test]
fn let_decl() {
    let pos = &mut Pos::new();
    lex_test(
        "let a = b;",
        &[
            (KeyWord(KeyWord::Let), pos.update(3)),
            (WhiteSpace, pos.update(1)),
            (Ident("a"), pos.update(1)),
            (WhiteSpace, pos.update(1)),
            (Eq, pos.update(1)),
            (WhiteSpace, pos.update(1)),
            (Ident("b"), pos.update(1)),
            (Semi, pos.update(1)),
        ],
    );
}
