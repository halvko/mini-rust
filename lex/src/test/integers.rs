use crate::*;

#[test]
fn int() {
    let input = "123";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec!((
            Token::Int("123"),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 3 }
            }
        )),
        lexed
    )
}

#[test]
fn typed_int() {
    let input = "123u32";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec!(
            (
                Token::Int("123"),
                Span {
                    start: Pos { line: 1, column: 0 },
                    end: Pos { line: 1, column: 3 }
                }
            ),
            (
                Token::Ident("u32"),
                Span {
                    start: Pos { line: 1, column: 3 },
                    end: Pos { line: 1, column: 6 }
                }
            )
        ),
        lexed
    )
}

#[test]
fn int_with_seperator() {
    let input = "123_000";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec!((
            Token::Int("123_000"),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 7 }
            }
        )),
        lexed
    )
}
