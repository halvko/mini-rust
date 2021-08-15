use std::{borrow::Cow, convert::TryFrom};

#[cfg(test)]
mod test;
pub struct TokenStream<'a> {
    input: &'a str,
    index: usize,
    pos: Pos,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Token<'a> {
    KeyWord(KeyWord),
    WhiteSpace,
    Bracket(Bracket),
    Square(Bracket),
    Curley(Bracket),
    Colon,
    Semi,
    Comma,
    Dot,
    Minus,
    Plus,
    Eq,
    Arrow,
    Ident(&'a str),
    StringLiteral(Cow<'a, str>),
    Int(&'a str),
    Float(&'a str),
    Error,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Bracket {
    Start,
    End,
}

#[derive(PartialEq, Eq, Debug)]
pub enum KeyWord {
    Fn,
    Let,
}

impl TryFrom<&str> for KeyWord {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "fn" => Ok(KeyWord::Fn),
            "let" => Ok(KeyWord::Let),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}

impl Pos {
    pub fn new() -> Self {
        Pos { line: 1, column: 0 }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new() -> Self {
        Span {
            start: Pos::new(),
            end: Pos::new(),
        }
    }
}

#[derive(Debug)]
enum State<'a> {
    // The empty state - nothing have been parsed yet
    Empty,
    // We found a token
    Sure(Token<'a>),
    // We have encountered whitespace, and are gonna parse until something else is encountered
    WhiteSpace,
    // We have encountered something that may either be an identifier or a keyword
    KeyOrIdent,
    MinusOrArrow,
    IntOrFloat,
    OptionFloat,
    Float,
    StringLiteral,
    CompleteStringLiteral,
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = (Token<'a>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let input = &self.input[self.index..];
        let mut indecies = input.char_indices();
        let mut state = State::Empty;
        let start = self.pos;

        while let Some((p, c)) = indecies.next() {
            let p = p + 1;
            if c == '\n' {
                self.pos.column = 0;
                self.pos.line += 1;
            } else {
                self.pos.column += 1;
            }

            let (s, done) = state.next(c);
            state = s;

            if let Some(i) = done {
                self.index += p - i;
                self.pos = Pos {
                    line: self.pos.line,
                    column: self.pos.column - i,
                };
                let token = state.token(&input[0..p - i])?;
                let span = Span {
                    start,
                    end: self.pos,
                };
                return Some((token, span));
            }
        }

        self.index = self.input.len();
        let token = state.token(input)?;
        let span = Span {
            start,
            end: self.pos,
        };
        Some((token, span))
    }
}

impl<'a> State<'a> {
    fn next(self, c: char) -> (Self, Option<usize>) {
        use State::*;
        match self {
            Sure(_) => unreachable!(),
            Empty => match c {
                'a'..='z' | 'A'..='Z' | '_' => (KeyOrIdent, None),
                '0'..='9' => (IntOrFloat, None),
                '(' => (Sure(Token::Bracket(Bracket::Start)), Some(0)),
                ')' => (Sure(Token::Bracket(Bracket::End)), Some(0)),
                '[' => (Sure(Token::Square(Bracket::Start)), Some(0)),
                ']' => (Sure(Token::Square(Bracket::End)), Some(0)),
                '{' => (Sure(Token::Curley(Bracket::Start)), Some(0)),
                '}' => (Sure(Token::Curley(Bracket::End)), Some(0)),
                ':' => (Sure(Token::Colon), Some(0)),
                ';' => (Sure(Token::Semi), Some(0)),
                ',' => (Sure(Token::Comma), Some(0)),
                '=' => (Sure(Token::Eq), Some(0)),
                '+' => (Sure(Token::Plus), Some(0)),
                '-' => (MinusOrArrow, None),
                '.' => (Sure(Token::Dot), Some(0)),
                '"' => (StringLiteral, None),
                c if c.is_whitespace() => (WhiteSpace, None),
                _ => (Sure(Token::Error), Some(0)),
            },
            WhiteSpace => (
                WhiteSpace,
                match c {
                    c if c.is_whitespace() => None,
                    _ => Some(1),
                },
            ),
            KeyOrIdent => (
                KeyOrIdent,
                match c {
                    'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => None,
                    _ => Some(1),
                },
            ),
            MinusOrArrow => match c {
                '>' => (Sure(Token::Arrow), Some(0)),
                _ => (MinusOrArrow, Some(1)),
            },
            IntOrFloat => match c {
                '0'..='9' | '_' => (IntOrFloat, None),
                '.' => (OptionFloat, None),
                _ => (IntOrFloat, Some(1)),
            },
            OptionFloat => match c {
                '0'..='9' => (Float, None),
                'a'..='z' | 'A'..='Z' | '_' => (IntOrFloat, Some(2)),
                _ => (Float, Some(1)),
            },
            Float => (
                Float,
                match c {
                    '0'..='9' | '_' => None,
                    _ => Some(1),
                },
            ),
            StringLiteral => match c {
                '"' => (CompleteStringLiteral, Some(0)),
                _ => (StringLiteral, None),
            },
            CompleteStringLiteral => unreachable!(),
        }
    }

    fn token(self, token_str: &'a str) -> Option<Token<'a>> {
        use State::*;
        Some(match self {
            Empty => return None,
            Sure(token) => token,
            WhiteSpace => Token::WhiteSpace,
            KeyOrIdent => {
                if let Ok(k) = KeyWord::try_from(token_str) {
                    Token::KeyWord(k)
                } else {
                    Token::Ident(token_str)
                }
            }
            MinusOrArrow => Token::Minus,
            IntOrFloat => Token::Int(token_str),
            OptionFloat => Token::Float(token_str),
            Float => Token::Float(token_str),
            StringLiteral => Token::Error,
            CompleteStringLiteral => {
                unescape_string(Cow::Borrowed(&token_str[1..token_str.len() - 1]))
                    .map(Token::StringLiteral)
                    .unwrap_or(Token::Error)
            }
        })
    }
}

fn unescape_string(s: Cow<'_, str>) -> Option<Cow<'_, str>> {
    if !s.contains("\\") {
        Some(s)
    } else {
        let mut ret = String::new();
        let mut escaped = false;
        for c in s.chars() {
            if escaped {
                match c {
                    '\\' => ret.push('\\'),
                    'n' => ret.push('\n'),
                    'r' => ret.push('\r'),
                    't' => ret.push('\t'),
                    _ => return None,
                }
                escaped = false
            }
        }
        Some(Cow::Owned(ret))
    }
}

pub fn lex(input: &str) -> TokenStream {
    TokenStream {
        input,
        index: 0,
        pos: Pos::new(),
    }
}
