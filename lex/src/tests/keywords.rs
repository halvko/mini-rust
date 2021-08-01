use crate::*;

#[test]
fn lex_fn() {
    let input = "fn";
    let lexed = lex(input).collect::<Vec<_>>();
    assert_eq!(
        vec![(
            Token::KeyWord(KeyWord::Fn),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 2 }
            }
        )],
        lexed
    )
}

#[test]
fn lex_let() {
    let input = "let";
    let mut lexed = lex(input);
    assert_eq!(
        Some((
            Token::KeyWord(KeyWord::Let),
            Span {
                start: Pos { line: 1, column: 0 },
                end: Pos { line: 1, column: 3 }
            }
        )),
        lexed.next()
    );
    assert!(lexed.next().is_none())
}
