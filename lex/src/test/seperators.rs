use crate::*;

#[test]
fn start_bracket() {
    let input = "(";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Bracket(Bracket::Start), pos.update(1))], lexed)
}

#[test]
fn end_bracket() {
    let input = ")";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Bracket(Bracket::End), pos.update(1))], lexed)
}

#[test]
fn start_square() {
    let input = "[";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Square(Bracket::Start), pos.update(1))], lexed)
}

#[test]
fn end_square() {
    let input = "]";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Square(Bracket::End), pos.update(1))], lexed)
}

#[test]
fn start_curley() {
    let input = "{";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Curley(Bracket::Start), pos.update(1))], lexed)
}

#[test]
fn end_curley() {
    let input = "}";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Curley(Bracket::End), pos.update(1))], lexed)
}

#[test]
fn colon() {
    let input = ":";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Colon, pos.update(1))], lexed)
}

#[test]
fn semi() {
    let input = ";";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Semi, pos.update(1))], lexed)
}

#[test]
fn comma() {
    let input = ",";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Comma, pos.update(1))], lexed)
}

#[test]
fn dot() {
    let input = ".";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Dot, pos.update(1))], lexed)
}

#[test]
fn arrow() {
    let input = "->";
    let lexed = lex(input).collect::<Vec<_>>();
    let pos = &mut Pos::new();
    assert_eq!(vec![(Token::Arrow, pos.update(2))], lexed)
}
