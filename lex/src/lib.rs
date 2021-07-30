use std::convert::TryFrom;

#[cfg(test)]
mod tests;

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
    Arrow,
    Minus,
    Eq,
    Ident(&'a str),
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

#[derive(Debug)]
enum State {
    // The empty state - nothing have been parsed yet
    Empty,
    // We have encountered whitespace, and are gonna parse until something else is encountered
    WhiteSpace,
    // We have encountered something that may either be an identifier or a keyword
    KeyOrIdent,
    MinusOrArrow,
}

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
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

impl<'a> Iterator for TokenStream<'a> {
    type Item = (Token<'a>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        let input = &self.input[self.index..];
        let mut indecies = input.char_indices();
        let mut state = State::Empty;
        let mut token_str = &input[..0];
        let mut span = Span {
            start: self.pos,
            end: self.pos,
        };

        while let Some((p, c)) = indecies.next() {
            if c == '\n' {
                span.end.line += 1;
                span.end.column = 0;
            } else {
                span.end.column += 1;
            }
            match state {
                State::Empty => match c {
                    'a'..='z' | 'A'..='Z' | '_' => state = State::KeyOrIdent,
                    '(' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Bracket(Bracket::Start), span));
                    }
                    ')' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Bracket(Bracket::End), span));
                    }
                    '[' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Square(Bracket::Start), span));
                    }
                    ']' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Square(Bracket::End), span));
                    }
                    '{' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Curley(Bracket::Start), span));
                    }
                    '}' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Curley(Bracket::End), span));
                    }
                    ':' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Colon, span));
                    }
                    ';' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Semi, span));
                    }
                    ',' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Comma, span));
                    }
                    '=' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Eq, span));
                    }
                    '-' => {
                        state = State::MinusOrArrow;
                    }
                    c if c.is_whitespace() => state = State::WhiteSpace,
                    c => {
                        let skip_bytes = c.len_utf8();
                        self.index = self.index + p + skip_bytes;
                        return Some((Token::Error, span));
                    }
                },
                State::WhiteSpace => {
                    if c.is_whitespace() {
                    } else {
                        self.index = self.index + p;
                        return Some((Token::WhiteSpace, span));
                    }
                }
                State::KeyOrIdent => match c {
                    'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {}
                    _ => {
                        self.index = self.index + p;
                        if let Ok(k) = KeyWord::try_from(token_str) {
                            return Some((Token::KeyWord(k), span));
                        } else {
                            return Some((Token::Ident(token_str), span));
                        }
                    }
                },
                State::MinusOrArrow => match c {
                    '>' => {
                        self.index = self.index + p + 1;
                        return Some((Token::Arrow, span));
                    }
                    _ => {
                        self.index = self.index + p;
                        return Some((Token::Minus, span));
                    }
                },
            }
            token_str = &input[..=p]
        }

        // No characters were left in the string
        self.index = self.input.len();
        match state {
            State::Empty => None,
            State::KeyOrIdent => {
                if let Ok(k) = KeyWord::try_from(token_str) {
                    Some((Token::KeyWord(k), span))
                } else {
                    Some((Token::Ident(token_str), span))
                }
            }
            State::WhiteSpace => Some((Token::WhiteSpace, span)),
            State::MinusOrArrow => Some((Token::Minus, span)),
        }
    }
}

pub fn lex(input: &str) -> TokenStream {
    TokenStream {
        input,
        index: 0,
        pos: Pos { line: 0, column: 0 },
    }
}
