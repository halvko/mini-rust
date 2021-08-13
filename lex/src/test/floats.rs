use super::prelude::*;

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
    let pos = &mut Pos::new();
    lex_test(
        "123.123f32",
        &[
            (Token::Float("123.123"), pos.update(7)),
            (Token::Ident("f32"), pos.update(3)),
        ],
    );
}

#[test]
fn float_with_seperator() {
    let pos = &mut Pos::new();
    lex_test("1_230.123", &[(Token::Float("1_230.123"), pos.update(9))]);
}

#[test]
fn almost_float() {
    let pos = &mut Pos::new();
    lex_test(
        "1_230._123",
        &[
            (Token::Int("1_230"), pos.update(5)),
            (Token::Dot, pos.update(1)),
            (Token::Ident("_123"), pos.update(4)),
        ],
    );
}
