use super::prelude::*;

#[test]
fn empty() {
    let pos = &mut Pos::new();
    lex_test("\"\"", &[(StringLiteral(Cow::Borrowed("")), pos.update(2))]);
}
