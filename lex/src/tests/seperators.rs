use crate::*;

#[test]
fn start_bracket() {
    let input = "(";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Bracket(Bracket::Start),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn end_bracket() {
    let input = ")";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Bracket(Bracket::End),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn start_square() {
    let input = "[";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Square(Bracket::Start),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn end_square() {
    let input = "]";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Square(Bracket::End),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn start_curley() {
    let input = "{";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Curley(Bracket::Start),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn end_curley() {
    let input = "}";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Curley(Bracket::End),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn colon() {
    let input = ":";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Colon,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn semi() {
    let input = ";";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Semi,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn comma() {
    let input = ",";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Comma,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn dot() {
    let input = ".";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Dot,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn arrow() {
    let input = "->";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Arrow,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 2 }
            }
        )],
        lexed
    )
}
