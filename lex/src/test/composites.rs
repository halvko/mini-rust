use crate::test::update_span;
use crate::*;

#[test]
fn fn_def() {
    let input = "fn lul(){}";
    let mut lexed = lex(input);
    let span = &mut Span::new();
    assert_eq!(
        Some((Token::KeyWord(KeyWord::Fn), update_span(span, 2, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("lul"), update_span(span, 3, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Bracket(Bracket::Start), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Bracket(Bracket::End), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Curley(Bracket::Start), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Curley(Bracket::End), update_span(span, 1, 0))),
        lexed.next()
    );
    assert!(lexed.next().is_none())
}

#[test]
fn fn_with_args() {
    let input = "fn lul(a: u16, b: String){}";
    let mut lexed = lex(input);
    let span = &mut Span::new();
    assert_eq!(
        Some((Token::KeyWord(KeyWord::Fn), update_span(span, 2, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("lul"), update_span(span, 3, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Bracket(Bracket::Start), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("a"), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(Some((Token::Colon, update_span(span, 1, 0))), lexed.next());
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("u16"), update_span(span, 3, 0))),
        lexed.next()
    );
    assert_eq!(Some((Token::Comma, update_span(span, 1, 0))), lexed.next());
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("b"), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(Some((Token::Colon, update_span(span, 1, 0))), lexed.next());
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("String"), update_span(span, 6, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Bracket(Bracket::End), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Curley(Bracket::Start), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Curley(Bracket::End), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(None, lexed.next());
}

#[test]
fn fn_with_return() {
    let input = "fn argh() -> u16 {}";
    let mut lexed = lex(input);
    let span = &mut Span {
        start: Pos { line: 1, column: 0 },
        end: Pos { line: 1, column: 0 },
    };

    assert_eq!(
        Some((Token::KeyWord(KeyWord::Fn), update_span(span, 2, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("argh"), update_span(span, 4, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Bracket(Bracket::Start), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Bracket(Bracket::End), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(Some((Token::Arrow, update_span(span, 2, 0))), lexed.next());
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("u16"), update_span(span, 3, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Curley(Bracket::Start), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Curley(Bracket::End), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(None, lexed.next());
}

#[test]
fn let_decl() {
    let input = "let a = b;";
    let mut lexed = lex(input);
    let span = &mut Span::new();
    assert_eq!(
        Some((Token::KeyWord(KeyWord::Let), update_span(span, 3, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("a"), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(Some((Token::Eq, update_span(span, 1, 0))), lexed.next());
    assert_eq!(
        Some((Token::WhiteSpace, update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(
        Some((Token::Ident("b"), update_span(span, 1, 0))),
        lexed.next()
    );
    assert_eq!(Some((Token::Semi, update_span(span, 1, 0))), lexed.next());
    assert_eq!(None, lexed.next());
}
