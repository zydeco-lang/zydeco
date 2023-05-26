use super::*;
#[test]
fn it_works_1() {
    let source = "!(!1)";
    let mut arena = syntax::Context::default();
    let t = parser::SingleTermParser::new()
        .parse(&source, &mut arena, lexer::Lexer::new(&source))
        .unwrap();
    assert!(arena.terms.get(t).is_some());
}
#[test]
fn it_works_2() {
    let source = "main { let x = 1 in ! exit x } end";
    let mut arena = syntax::Context::default();
    let _t = parser::TopLevelParser::new()
        .parse(&source, &mut arena, lexer::Lexer::new(&source))
        .unwrap();
}
