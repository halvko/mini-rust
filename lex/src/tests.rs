mod floats;
mod integers;
mod keywords;
mod operators;
mod seperators;

use crate::*;

fn whitespace() {
    let input = " ";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::WhiteSpace,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

fn newline() {
    let input = "\n";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::WhiteSpace,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 2, column: 0 }
            }
        )],
        lexed
    )
}

fn multiple_whitespace() {
    let input = " \n ";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::WhiteSpace,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 2, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn lex_ident() {
    let input = "saoecri";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Ident("saoecri"),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 7 }
            }
        )],
        lexed
    )
}

#[test]
fn lex_two_fn() {
    let input = "fn fn";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![
            (
                Token::KeyWord(KeyWord::Fn),
                Span {
                    start: Pos { line: 1, column: 0 },
                    end: Pos { line: 1, column: 2 }
                }
            ),
            (
                Token::WhiteSpace,
                Span {
                    start: Pos { line: 1, column: 2 },
                    end: Pos { line: 1, column: 3 }
                }
            ),
            (
                Token::KeyWord(KeyWord::Fn),
                Span {
                    start: Pos { line: 1, column: 3 },
                    end: Pos { line: 1, column: 5 }
                }
            )
        ],
        lexed
    )
}

#[test]
fn lex_two_idents() {
    let input = "fna fni";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![
            (
                Token::Ident("fna"),
                Span {
                    start: Pos { line: 1, column: 0 },
                    end: Pos { line: 1, column: 3 }
                }
            ),
            (
                Token::WhiteSpace,
                Span {
                    start: Pos { line: 1, column: 3 },
                    end: Pos { line: 1, column: 4 }
                }
            ),
            (
                Token::Ident("fni"),
                Span {
                    start: Pos { line: 1, column: 4 },
                    end: Pos { line: 1, column: 7 }
                }
            ),
        ],
        lexed
    )
}

#[test]
fn fn_def() {
    let input = "fn lul(){}";
    let mut lexed = lex(input);
    assert_eq!(
        Some((
            Token::KeyWord(KeyWord::Fn),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 2 }
            }
        )),
        lexed.next()
    );
    assert_eq!(
        Some((
            Token::WhiteSpace,
            Span {
                start: Pos { line: 1, column: 2 },
                end: Pos { line: 1, column: 3 },
            },
        )),
        lexed.next()
    );
    assert_eq!(
        Some((
            Token::Ident("lul"),
            Span {
                start: Pos { line: 1, column: 3 },
                end: Pos { line: 1, column: 6 },
            }
        )),
        lexed.next()
    );
    assert_eq!(
        Some((
            Token::Bracket(Bracket::Start),
            Span {
                start: Pos { line: 1, column: 6 },
                end: Pos { line: 1, column: 7 },
            },
        )),
        lexed.next()
    );
    assert_eq!(
        Some((
            Token::Bracket(Bracket::End),
            Span {
                start: Pos { line: 1, column: 7 },
                end: Pos { line: 1, column: 8 },
            },
        )),
        lexed.next()
    );
    assert_eq!(
        Some((
            Token::Curley(Bracket::Start),
            Span {
                start: Pos { line: 1, column: 8 },
                end: Pos { line: 1, column: 9 },
            },
        )),
        lexed.next()
    );
    assert_eq!(
        Some((
            Token::Curley(Bracket::End),
            Span {
                start: Pos { line: 1, column: 9 },
                end: Pos {
                    line: 1,
                    column: 10,
                },
            },
        )),
        lexed.next()
    );
    assert!(lexed.next().is_none())
}

#[test]
fn fn_with_args() {
    let input = "fn lul(a: u16, b: String){}";
    let lexed = lex(input).map(|(t, _)| t).collect::<Vec<_>>();
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
    let lexed = lex(input).map(|(t, _)| t).collect::<Vec<_>>();
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
    let lexed = lex(input).map(|(t, _)| t).collect::<Vec<_>>();
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
