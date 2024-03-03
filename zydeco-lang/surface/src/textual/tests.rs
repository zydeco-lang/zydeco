use zydeco_utils::span::LocationCtx;

use super::*;
#[test]
fn parsing_1() {
    let source = "!(!1)";
    let mut ctx = syntax::Ctx::default();
    let t = parser::SingleTermParser::new()
        .parse(&source, &LocationCtx::Plain, &mut ctx, lexer::Lexer::new(&source))
        .unwrap();
    assert!(ctx.terms.get(t).is_some());
}
#[test]
fn parsing_2() {
    let source = "main { let x = 1 in ! exit x } end";
    let mut ctx = syntax::Ctx::default();
    let _t = parser::TopLevelParser::new()
        .parse(&source, &LocationCtx::Plain, &mut ctx, lexer::Lexer::new(&source))
        .unwrap();
}
