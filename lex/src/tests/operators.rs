use crate::*;

#[test]
fn minus() {
    let input = "-";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Minus,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

#[test]
fn plus() {
    let input = "+";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Plus,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}

fn eq() {
    let input = "=";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::Eq,
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 1 }
            }
        )],
        lexed
    )
}
