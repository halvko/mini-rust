enum TokenKind {
    Keyword(Keyword),
}
enum Keyword {
    FN,
}

struct Span {}

struct Token {
    kind: TokenKind,
    span: Span,
}

pub struct Lexer {
    cheat: Vec<Token>,
}

pub fn lex(input: &str) -> Lexer {
    let mut chars = input.chars();
    let mut res = Vec::new();
    while let Some(t) = lex_token(&mut chars) {
        res.push(t);
    }
    Lexer { cheat: res }
}

fn lex_token(input: &mut impl Iterator<Item = char>) -> Option<Token> {
    let Some(c) = input.next() else {
        return None;
    };
    let mut a;
    if (a = 1) {
        unreachable!();
    };
}

fn lex_kw_or_ident(input: &mut impl Iterator<Item = char>) -> Option<Token> {
    l
}

fn main() {
    println!("Hello, world!");
}
