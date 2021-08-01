use crate::*;

#[test]
fn float() {
    let input = "123.123";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec!((
            Token::Float("123.123"),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 7 }
            }
        )),
        lexed
    )
}

#[test]
fn typed_float() {
    let input = "123.123f32";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec!((
            Token::Float("123.123f32"),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos {
                    line: 1,
                    column: 10
                }
            }
        )),
        lexed
    )
}

#[test]
fn float_with_seperator() {
    let input = "1_230.123";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec!((
            Token::Float("1_230.123"),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 9 }
            }
        )),
        lexed
    )
}

#[test]
fn almost_float() {
    let input = "1_230._123";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec!(
            (
                Token::Int("1_230"),
                Span {
                    start: Pos { line: 1, column: 0 },
                    end: Pos { line: 1, column: 5 }
                }
            ),
            (
                Token::Dot,
                Span {
                    start: Pos { line: 1, column: 5 },
                    end: Pos { line: 1, column: 6 }
                }
            ),
            (
                Token::Ident("_123"),
                Span {
                    start: Pos { line: 1, column: 6 },
                    end: Pos {
                        line: 1,
                        column: 10
                    }
                }
            ),
        ),
        lexed
    )
}
