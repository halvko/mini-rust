mod composites;
mod floats;
mod integers;
mod keywords;
mod operators;
mod seperators;
mod string_literals;

impl Pos {
    fn update(self: &mut Pos, len: usize) -> Span {
        let start = *self;
        self.column += len;
        self.offset += len;
        Span { start, end: *self }
    }

    fn update_ln(self: &mut Pos, lines: usize, column: usize, offset: usize) -> Span {
        let start = *self;
        let end = Pos {
            line: start.line + lines,
            column,
            offset: start.offset + offset,
        };
        *self = end;
        Span { start, end }
    }
}

fn lex_test(input: &str, expected: &[(Token, Span)]) {
    let lexed = lex(input).take(expected.len() + 1).collect::<Vec<_>>();
    assert_eq!(expected, &lexed);
    assert_eq!(None, lexed.get(expected.len()));
}

mod prelude {
    pub(super) use super::lex_test;
    pub use crate::*;
    pub use Token::*;
}
use prelude::*;

#[test]
fn whitespace() {
    let pos = &mut Pos::new();
    lex_test(" ", &[(WhiteSpace, pos.update(1))]);
}

#[test]
fn newline() {
    let pos = &mut Pos::new();
    lex_test("\n", &[(WhiteSpace, pos.update_ln(1, 0, 1))]);
}

#[test]
fn multiple_whitespace() {
    let pos = &mut Pos::new();
    lex_test(" \n ", &[(WhiteSpace, pos.update_ln(1, 1, 3))]);
}

#[test]
fn lex_ident() {
    let pos = &mut Pos::new();
    lex_test("saoecri", &[(Ident("saoecri"), pos.update(7))]);
}

#[test]
fn lex_two_fn() {
    let pos = &mut Pos::new();
    lex_test(
        "fn fn",
        &[
            (KeyWord(KeyWord::Fn), pos.update(2)),
            (WhiteSpace, pos.update(1)),
            (KeyWord(KeyWord::Fn), pos.update(2)),
        ],
    );
}

#[test]
fn lex_two_idents() {
    let pos = &mut Pos::new();
    lex_test(
        "fna fni",
        &[
            (Ident("fna"), pos.update(3)),
            (WhiteSpace, pos.update(1)),
            (Ident("fni"), pos.update(3)),
        ],
    );
}
