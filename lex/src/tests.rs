use super::*;

#[test]
fn lex_fn() {
    let input = "fn";
    let lexed = lex(input).map(|(t, s)| t).collect::<Vec<_>>();
    assert_eq!(vec![Token::KeyWord(KeyWord::Fn)], lexed)
}

#[test]
fn lex_ident() {
    let input = "saoecri";
    let lexed = lex(input).map(|(t, s)| t).collect::<Vec<_>>();
    assert_eq!(vec![Token::Ident("saoecri")], lexed)
}

#[test]
fn lex_two_fn() {
    let input = "fn fn";
    let lexed = lex(input).map(|(t, s)| t).collect::<Vec<_>>();
    assert_eq!(
        vec![
            Token::KeyWord(KeyWord::Fn),
            Token::WhiteSpace,
            Token::KeyWord(KeyWord::Fn)
        ],
        lexed
    )
}

#[test]
fn lex_two_idents() {
    let input = "fna fni";
    let lexed = lex(input).map(|(t, s)| t).collect::<Vec<_>>();
    assert_eq!(
        vec![Token::Ident("fna"), Token::WhiteSpace, Token::Ident("fni")],
        lexed
    )
}

#[test]
fn fn_def() {
    let input = "fn lul(){}";
    let lexed = lex(input).map(|(t, s)| t).collect::<Vec<_>>();
    assert_eq!(
        vec![
            Token::KeyWord(KeyWord::Fn),
            Token::WhiteSpace,
            Token::Ident("lul"),
            Token::Bracket(Bracket::Start),
            Token::Bracket(Bracket::End),
            Token::Curley(Bracket::Start),
            Token::Curley(Bracket::End)
        ],
        lexed
    )
}

#[test]
fn fn_with_args() {
    let input = "fn lul(a: u16, b: String){}";
    let lexed = lex(input).map(|(t, s)| t).collect::<Vec<_>>();
    assert_eq!(
        vec!(
            Token::KeyWord(KeyWord::Fn),
            Token::WhiteSpace,
            Token::Ident("lul"),
            Token::Bracket(Bracket::Start),
            Token::Ident("a"),
            Token::Colon,
            Token::WhiteSpace,
            Token::Ident("u16"),
            Token::Comma,
            Token::WhiteSpace,
            Token::Ident("b"),
            Token::Colon,
            Token::WhiteSpace,
            Token::Ident("String"),
            Token::Bracket(Bracket::End),
            Token::Curley(Bracket::Start),
            Token::Curley(Bracket::End),
        ),
        lexed
    )
}

#[test]
fn fn_with_return() {
    let input = "fn argh() -> u16 {}";
    let lexed = lex(input).map(|(t, s)| t).collect::<Vec<_>>();
    assert_eq!(
        vec!(
            Token::KeyWord(KeyWord::Fn),
            Token::WhiteSpace,
            Token::Ident("argh"),
            Token::Bracket(Bracket::Start),
            Token::Bracket(Bracket::End),
            Token::WhiteSpace,
            Token::Arrow,
            Token::WhiteSpace,
            Token::Ident("u16"),
            Token::WhiteSpace,
            Token::Curley(Bracket::Start),
            Token::Curley(Bracket::End),
        ),
        lexed
    )
}

#[test]
fn let_decl() {
    let input = "let a = b;";
    let lexed = lex(input).map(|(t, s)| t).collect::<Vec<_>>();
    assert_eq!(
        vec!(
            Token::KeyWord(KeyWord::Let),
            Token::WhiteSpace,
            Token::Ident("a"),
            Token::WhiteSpace,
            Token::Eq,
            Token::WhiteSpace,
            Token::Ident("b"),
            Token::Semi,
        ),
        lexed
    )
}
