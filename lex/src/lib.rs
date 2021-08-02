use std::{borrow::Cow, convert::TryFrom};

#[cfg(test)]
mod test;

/*
fn pos_iter<'a>(s: &'a str) -> impl Iterator<Item = (char, Pos)> + 'a {
    PosIter {
        iter: s.chars(),
        pos: Pos { line: 0, column: 0 },
    }
}

pub struct PosIter<I> {
    iter: I,
    pos: Pos,
}

impl<I: Iterator<Item = char>> Iterator for PosIter<I> {
    type Item = (char, Pos);
    fn next(&mut self) -> Option<Self::Item> {
        let c = self.iter.next()?;
        let pos = self.pos.clone();
        if c == '\n' {
            self.pos.line += 1;
            self.pos.column = 0;
        } else {
            self.pos.column += 1;
        }
        Some((c, pos))
    }
}
*/

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new() -> Self {
        Span {
            start: Pos { line: 1, column: 0 },
            end: Pos { line: 1, column: 0 },
        }
    }
}

/*impl<'a> TokenStream<'a> {
    fn next_token(&mut self) -> Option<<Self as Iterator>::Item> {
        let input = &self.input[self.index..];
        let mut indecies = input.char_indices();
        let (p, c) = indecies.next()?;
        match c {
            'a'..='z' | 'A'..='Z' | '_' => {
                let (q, id) = match indecies
                    .skip_while(|(_, c)| {
                        if let 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' = c {
                            true
                        } else {
                            false
                        }
                    })
                    .next()
                {
                    Some((q, _)) => (q, &input[p..p + q]),
                    None => (input.len() - p, &input[p..]),
                };
                Some((Token::Ident(id), unimplemented!(q)))
            }
            _ => unimplemented!(),
        }
    }
}*/

#[derive(Debug)]
enum State {
    // The empty state - nothing have been parsed yet
    Empty,
    // We have encountered whitespace, and are gonna parse until something else is encountered
    WhiteSpace,
    // We have encountered something that may either be an identifier or a keyword
    KeyOrIdent,
    MinusOrArrow,
    IntOrFloat,
    OptionFloat,
    StringLiteral,
    StringLiteralEscaped,
    Float,
    Int,
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = (Token<'a>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let input = &self.input[self.index..];
        let mut indecies = input.char_indices();
        let mut state = State::Empty;
        let mut token_str = &input[..0];
        let start = self.pos;

        while let Some((p, c)) = indecies.next() {
            match state {
                State::Empty => match c {
                    'a'..='z' | 'A'..='Z' | '_' => state = State::KeyOrIdent,
                    '0'..='9' => state = State::IntOrFloat,
                    '(' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Bracket(Bracket::Start),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    ')' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Bracket(Bracket::End),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    '[' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Square(Bracket::Start),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    ']' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Square(Bracket::End),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    '{' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Curley(Bracket::Start),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    '}' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Curley(Bracket::End),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    ':' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Colon,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    ';' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Semi,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    ',' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Comma,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    '=' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Eq,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    '+' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Plus,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    '-' => {
                        state = State::MinusOrArrow;
                    }
                    '.' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Dot,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    '"' => {
                        state = State::StringLiteral;
                    }
                    c if c.is_whitespace() => state = State::WhiteSpace,
                    c => {
                        let skip_bytes = c.len_utf8();
                        self.index += p + skip_bytes;
                        self.pos.column += 1;
                        return Some((
                            Token::Error,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                },
                State::WhiteSpace => {
                    if c.is_whitespace() {
                    } else {
                        self.index += p;
                        return Some((
                            Token::WhiteSpace,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                }
                State::KeyOrIdent => match c {
                    'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {}
                    _ => {
                        self.index += p;
                        if let Ok(k) = KeyWord::try_from(token_str) {
                            return Some((
                                Token::KeyWord(k),
                                Span {
                                    start,
                                    end: self.pos,
                                },
                            ));
                        } else {
                            return Some((
                                Token::Ident(token_str),
                                Span {
                                    start,
                                    end: self.pos,
                                },
                            ));
                        }
                    }
                },
                State::MinusOrArrow => match c {
                    '>' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return Some((
                            Token::Arrow,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    _ => {
                        self.index += p;
                        return Some((
                            Token::Minus,
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                },
                State::IntOrFloat => match c {
                    '0'..='9' | '_' => {}
                    'a'..='z' | 'A'..='Z' => state = State::Int,
                    '.' => state = State::OptionFloat,
                    _ => {
                        self.index += p;
                        return Some((
                            Token::Int(token_str),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                },
                State::Int => match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {}
                    _ => {
                        self.index += p;
                        return Some((
                            Token::Int(token_str),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                },
                State::OptionFloat => match c {
                    '0'..='9' => state = State::Float,
                    'a'..='z' | 'A'..='Z' | '_' => {
                        self.index += p - 1;
                        self.pos.column -= 1;
                        return Some((
                            Token::Int(&token_str[..token_str.len() - 1]),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ));
                    }
                    _ => {
                        return Some((
                            Token::Float(token_str),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ))
                    }
                },
                State::Float => match c {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {}
                    _ => {
                        return Some((
                            Token::Float(token_str),
                            Span {
                                start,
                                end: self.pos,
                            },
                        ))
                    }
                },
                State::StringLiteral => match c {
                    '"' => {
                        self.index += p + 1;
                        self.pos.column += 1;
                        return match unescape_string(Cow::Borrowed(&token_str[1..])) {
                            Some(s) => Some((
                                Token::StringLiteral(s),
                                Span {
                                    start,
                                    end: self.pos,
                                },
                            )),
                            None => Some((
                                Token::Error,
                                Span {
                                    start,
                                    end: self.pos,
                                },
                            )),
                        };
                    }
                    '\\' => state = State::StringLiteralEscaped,
                    _ => {}
                },
                State::StringLiteralEscaped => match c {
                    _ => state = State::StringLiteral,
                },
            }

            token_str = &input[..=p];
            if c == '\n' {
                self.pos.line += 1;
                self.pos.column = 0;
            } else {
                self.pos.column += 1;
            }
        }

        // No characters were left in the string
        self.index = self.input.len();
        match state {
            State::Empty => None,
            State::KeyOrIdent => {
                if let Ok(k) = KeyWord::try_from(token_str) {
                    Some((
                        Token::KeyWord(k),
                        Span {
                            start,
                            end: self.pos,
                        },
                    ))
                } else {
                    Some((
                        Token::Ident(token_str),
                        Span {
                            start,
                            end: self.pos,
                        },
                    ))
                }
            }
            State::WhiteSpace => Some((
                Token::WhiteSpace,
                Span {
                    start,
                    end: self.pos,
                },
            )),
            State::MinusOrArrow => Some((
                Token::Minus,
                Span {
                    start,
                    end: self.pos,
                },
            )),
            State::IntOrFloat | State::Int => Some((
                Token::Int(token_str),
                Span {
                    start,
                    end: self.pos,
                },
            )),
            State::OptionFloat | State::Float => Some((
                Token::Float(token_str),
                Span {
                    start,
                    end: self.pos,
                },
            )),
            State::StringLiteral | State::StringLiteralEscaped => Some((
                Token::Error,
                Span {
                    start,
                    end: self.pos,
                },
            )),
        }
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
        pos: Pos { line: 1, column: 0 },
    }
}
