use super::*;
#[test]
fn it_works_1() {
    let source = "!(!1)";
    let mut ctx = syntax::Ctx::default();
    let t = parser::SingleTermParser::new()
        .parse(&source, &mut ctx, lexer::Lexer::new(&source))
        .unwrap();
    assert!(ctx.terms.get(t).is_some());
}
#[test]
fn it_works_2() {
    let source = "main { let x = 1 in ! exit x } end";
    let mut ctx = syntax::Ctx::default();
    let _t =
        parser::TopLevelParser::new().parse(&source, &mut ctx, lexer::Lexer::new(&source)).unwrap();
}
