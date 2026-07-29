use crate::textual::{
    arena::TextualScope,
    syntax::{
        Ann, Appli, CoPatId, DeclId, DefId, Dtor, EntityId, Hole, Literal, Named, Paren, Parser,
        PatId, Pattern, Prod, Proj, Term, TermId,
    },
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

#[test]
fn parses_named_term_fields() {
    let source = "(x = 1, y = 2)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(fields)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized named tuple")
    };
    let fields = fields
        .iter()
        .map(|field| {
            let Term::Named(Named(name, body)) = &parser.arena.terms[field] else {
                panic!("expected a named term")
            };
            let Term::Lit(Literal::Int(value)) = &parser.arena.terms[body] else {
                panic!("expected an integer payload")
            };
            (name.plain(), *value)
        })
        .collect::<Vec<_>>();

    assert_eq!(fields, vec![("x".to_string(), 1), ("y".to_string(), 2)]);
}

#[test]
fn parses_comma_separated_named_terms_without_early_sorting() {
    let source = "(x = Int, y = String)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(fields)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized named type")
    };
    let fields = fields
        .iter()
        .map(|field| {
            let Term::Named(Named(name, body)) = &parser.arena.terms[field] else {
                panic!("expected a named type field")
            };
            let Term::Var(payload) = &parser.arena.terms[body] else {
                panic!("expected a type payload")
            };
            (name.plain(), payload.plain())
        })
        .collect::<Vec<_>>();

    assert_eq!(
        fields,
        vec![("x".to_string(), "Int".to_string()), ("y".to_string(), "String".to_string()),]
    );
}

#[test]
fn parses_named_product_type() {
    let source = "(x = Int) * (y = String)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Prod(Prod(left, right)) = &parser.arena.terms[&term] else {
        panic!("expected a product type")
    };
    let Term::Paren(Paren(left_fields)) = &parser.arena.terms[left] else {
        panic!("expected a parenthesized left component")
    };
    let [left_field] = left_fields.as_slice() else { panic!("expected one left component") };
    let Term::Named(Named(left_name, left_body)) = &parser.arena.terms[left_field] else {
        panic!("expected a named left component")
    };
    let Term::Var(left_type) = &parser.arena.terms[left_body] else {
        panic!("expected a left component type")
    };

    let Term::Paren(Paren(right_fields)) = &parser.arena.terms[right] else {
        panic!("expected a parenthesized right component")
    };
    let [right_field] = right_fields.as_slice() else { panic!("expected one right component") };
    let Term::Named(Named(right_name, right_body)) = &parser.arena.terms[right_field] else {
        panic!("expected a named right component")
    };
    let Term::Var(right_type) = &parser.arena.terms[right_body] else {
        panic!("expected a right component type")
    };

    assert_eq!(left_name.plain(), "x");
    assert_eq!(left_type.plain(), "Int");
    assert_eq!(right_name.plain(), "y");
    assert_eq!(right_type.plain(), "String");
}

#[test]
fn parses_named_term_payload_annotation() {
    let source = "(name = 1 : _)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(fields)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized named tuple")
    };
    let [field] = fields.as_slice() else { panic!("expected one named field") };
    let Term::Named(Named(name, body)) = &parser.arena.terms[field] else {
        panic!("expected a named term")
    };
    let Term::Ann(Ann { tm, ty }) = &parser.arena.terms[body] else {
        panic!("expected the field payload to be annotated")
    };

    assert_eq!(name.plain(), "name");
    assert!(matches!(parser.arena.terms[tm], Term::Lit(Literal::Int(1))));
    assert!(matches!(parser.arena.terms[ty], Term::Hole(Hole)));
}

#[test]
fn parses_chained_named_terms() {
    let source = "(outer = inner = 1)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(fields)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized named tuple")
    };
    let [field] = fields.as_slice() else { panic!("expected one named field") };
    let Term::Named(Named(outer, body)) = &parser.arena.terms[field] else {
        panic!("expected an outer named term")
    };
    let Term::Named(Named(inner, body)) = &parser.arena.terms[body] else {
        panic!("expected an inner named term")
    };

    assert_eq!(outer.plain(), "outer");
    assert_eq!(inner.plain(), "inner");
    assert!(matches!(parser.arena.terms[body], Term::Lit(Literal::Int(1))));
}

#[test]
fn parses_named_pattern_fields() {
    let source = "(x = left, y = right)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Pattern::Paren(Paren(fields)) = &parser.arena.pats[&pattern] else {
        panic!("expected a parenthesized named tuple pattern")
    };
    let fields = fields
        .iter()
        .map(|field| {
            let Pattern::Named(Named(name, body)) = &parser.arena.pats[field] else {
                panic!("expected a named pattern")
            };
            let Pattern::Var(payload) = &parser.arena.pats[body] else {
                panic!("expected a variable payload")
            };
            (name.plain(), parser.arena.defs[payload].plain())
        })
        .collect::<Vec<_>>();

    assert_eq!(
        fields,
        vec![("x".to_string(), "left".to_string()), ("y".to_string(), "right".to_string()),]
    );
}

#[test]
fn parses_named_pattern_payload_annotation() {
    let source = "(name = payload : _)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Pattern::Paren(Paren(fields)) = &parser.arena.pats[&pattern] else {
        panic!("expected a parenthesized named tuple pattern")
    };
    let [field] = fields.as_slice() else { panic!("expected one named field") };
    let Pattern::Named(Named(name, body)) = &parser.arena.pats[field] else {
        panic!("expected a named pattern")
    };
    let Pattern::Ann(Ann { tm, ty }) = &parser.arena.pats[body] else {
        panic!("expected the field payload to be annotated")
    };
    let Pattern::Var(payload) = &parser.arena.pats[tm] else {
        panic!("expected a variable payload")
    };

    assert_eq!(name.plain(), "name");
    assert_eq!(parser.arena.defs[payload].plain(), "payload");
    assert!(matches!(parser.arena.terms[ty], Term::Hole(Hole)));
}

#[test]
fn parses_chained_named_patterns() {
    let source = "(outer = inner = payload)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Pattern::Paren(Paren(fields)) = &parser.arena.pats[&pattern] else {
        panic!("expected a parenthesized named tuple pattern")
    };
    let [field] = fields.as_slice() else { panic!("expected one named field") };
    let Pattern::Named(Named(outer, body)) = &parser.arena.pats[field] else {
        panic!("expected an outer named pattern")
    };
    let Pattern::Named(Named(inner, body)) = &parser.arena.pats[body] else {
        panic!("expected an inner named pattern")
    };
    let Pattern::Var(payload) = &parser.arena.pats[body] else {
        panic!("expected a variable payload")
    };

    assert_eq!(outer.plain(), "outer");
    assert_eq!(inner.plain(), "inner");
    assert_eq!(parser.arena.defs[payload].plain(), "payload");
}

#[test]
fn parses_chained_named_projection() {
    let source = "rectangle/top_left/x";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Proj(Proj(inner, x)) = &parser.arena.terms[&term] else {
        panic!("expected an outer named projection")
    };
    let Term::Proj(Proj(receiver, top_left)) = &parser.arena.terms[inner] else {
        panic!("expected an inner named projection")
    };
    let Term::Var(rectangle) = &parser.arena.terms[receiver] else {
        panic!("expected a variable projection receiver")
    };

    assert_eq!(rectangle.plain(), "rectangle");
    assert_eq!(top_left.plain(), "top_left");
    assert_eq!(x.plain(), "x");
}

#[test]
fn named_projection_binds_tighter_than_application() {
    let source = "service/inspect rectangle/top_left";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::App(Appli(items)) = &parser.arena.terms[&term] else {
        panic!("expected an application")
    };
    let [function, argument] = items.as_slice() else { panic!("expected a binary application") };
    let Term::Proj(Proj(function, function_field)) = &parser.arena.terms[function] else {
        panic!("expected the application function to be a projection")
    };
    let Term::Var(function) = &parser.arena.terms[function] else {
        panic!("expected a variable function receiver")
    };
    let Term::Proj(Proj(receiver, field)) = &parser.arena.terms[argument] else {
        panic!("expected the application argument to be a projection")
    };
    let Term::Var(receiver) = &parser.arena.terms[receiver] else {
        panic!("expected a variable projection receiver")
    };

    assert_eq!(function.plain(), "service");
    assert_eq!(function_field.plain(), "inspect");
    assert_eq!(receiver.plain(), "rectangle");
    assert_eq!(field.plain(), "top_left");
}

#[test]
fn parses_chained_dot_elimination() {
    let source = "rectangle .top_left .x";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Dtor(Dtor(inner, x)) = &parser.arena.terms[&term] else {
        panic!("expected an outer dot elimination")
    };
    let Term::Dtor(Dtor(_, top_left)) = &parser.arena.terms[inner] else {
        panic!("expected an inner dot elimination")
    };

    assert_eq!(top_left.plain(), "top_left");
    assert_eq!(x.plain(), "x");
}
