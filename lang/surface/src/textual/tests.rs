use crate::textual::{
    arena::TextualScope,
    syntax::{CoPatId, DeclId, DefId, EntityId, Parser, PatId, TermId},
};
use zydeco_utils::{arena::IdAllocator, span::LocationCtx};

use super::*;

#[test]
fn textual_entities_retain_their_category_tags() {
    let mut allocator = IdAllocator::<TextualScope>::new();
    let def: DefId = allocator.alloc();
    let pat: PatId = allocator.alloc();
    let copat: CoPatId = allocator.alloc();
    let term: TermId = allocator.alloc();
    let decl: DeclId = allocator.alloc();

    assert!(matches!(EntityId::from(def), EntityId::Def(id) if id == def));
    assert!(matches!(EntityId::from(pat), EntityId::Pat(id) if id == pat));
    assert!(matches!(EntityId::from(copat), EntityId::CoPat(id) if id == copat));
    assert!(matches!(EntityId::from(term), EntityId::Term(id) if id == term));
    assert!(matches!(EntityId::from(decl), EntityId::Decl(id) if id == decl));
}

#[test]
fn parsing_1() {
    let source = "!(!1)";
    let mut parser = Parser::new();
    let t = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();
    println!("{:?}", &parser.arena.terms[&t]);
}
#[test]
fn parsing_2() {
    let source = "main { let x = 1 in ! exit x } end";
    let mut parser = Parser::new();
    let t = parser::TopLevelParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();
    println!("{:?}", t);
}
