use crate::textual::syntax::Parser;
use zydeco_utils::{arena::GlobalAlloc, span::LocationCtx};

use super::*;
#[test]
fn parsing_1() {
    let source = "!(!1)";
    let mut alloc = GlobalAlloc::new();
    let mut parser = Parser::new(&mut alloc);
    let t = parser::SingleTermParser::new()
        .parse(&source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(&source))
        .unwrap();
    println!("{:?}", &parser.arena.terms[t]);
}
#[test]
fn parsing_2() {
    let source = "main { let x = 1 in ! exit x } end";
    let mut alloc = GlobalAlloc::new();
    let mut parser = Parser::new(&mut alloc);
    let t = parser::TopLevelParser::new()
        .parse(&source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(&source))
        .unwrap();
    println!("{:?}", t);
}
