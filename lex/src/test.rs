mod composites;
mod floats;
mod integers;
mod keywords;
mod operators;
mod seperators;
mod string_literals;

fn update_span(span: &mut Span, len: usize, height: usize) -> Span {
    let start = span.end;
    let end = Pos {
        line: start.line + height,
        column: start.column + len,
    };
    span.start = start;
    span.end = end;
    span.clone()
}

use crate::*;

#[test]
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

#[test]
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

#[test]
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
