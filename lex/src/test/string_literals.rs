use crate::test::update_span;
use crate::*;

#[test]
fn empty() {
    let input = "\"\"";
    let mut lexed = lex(input);
    let span = &mut Span::new();
    assert_eq!(
        Some((
            Token::StringLiteral(Cow::Borrowed("")),
            update_span(span, 2, 0)
        )),
        lexed.next()
    );
    assert_eq!(None, lexed.next());
}
