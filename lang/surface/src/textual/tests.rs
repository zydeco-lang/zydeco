use crate::textual::{
    arena::TextualScope,
    syntax::{
        Ann, Appli, CoPatId, DeclId, DefId, Dtor, EntityId, Hole, Label, Literal, ManifestExists,
        Named, Paren, Parser, PatId, Pattern, Prod, Proj, Term, TermId,
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
fn parses_manifest_existential_with_a_punned_field_binder() {
    let source = "exists (= Counter as Int : VType) . Counter";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::ManifestExists(ManifestExists { binder, definition, kind, body }) =
        &parser.arena.terms[&term]
    else {
        panic!("expected a manifest existential")
    };
    let Pattern::Named(Named(field, binder)) = &parser.arena.pats[binder] else {
        panic!("expected a punned named binder")
    };
    let Pattern::Var(binder) = &parser.arena.pats[binder] else {
        panic!("expected the field payload to be a type variable")
    };
    let Term::Var(definition) = &parser.arena.terms[definition] else {
        panic!("expected a manifest definition")
    };
    let Term::Var(kind) = &parser.arena.terms[kind] else { panic!("expected a binder kind") };
    let Term::Var(body) = &parser.arena.terms[body] else { panic!("expected a package body") };

    assert_eq!(field.plain(), "Counter");
    assert_eq!(parser.arena.defs[binder].plain(), "Counter");
    assert_eq!(definition.plain(), "Int");
    assert_eq!(kind.plain(), "VType");
    assert_eq!(body.plain(), "Counter");
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
fn parses_labeled_product_type() {
    let source = "(x :: Int) * (y :: String)";
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
    let Term::Label(Label(left_name, left_body)) = &parser.arena.terms[left_field] else {
        panic!("expected a labeled left component")
    };
    let Term::Var(left_type) = &parser.arena.terms[left_body] else {
        panic!("expected a left component type")
    };

    let Term::Paren(Paren(right_fields)) = &parser.arena.terms[right] else {
        panic!("expected a parenthesized right component")
    };
    let [right_field] = right_fields.as_slice() else { panic!("expected one right component") };
    let Term::Label(Label(right_name, right_body)) = &parser.arena.terms[right_field] else {
        panic!("expected a labeled right component")
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
fn parses_chained_labels_right_associatively() {
    let source = "(outer :: inner :: Int)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(fields)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized labeled term")
    };
    let [field] = fields.as_slice() else { panic!("expected one labeled term") };
    let Term::Label(Label(outer, body)) = &parser.arena.terms[field] else {
        panic!("expected an outer label")
    };
    let Term::Label(Label(inner, body)) = &parser.arena.terms[body] else {
        panic!("expected an inner label")
    };
    let Term::Var(payload) = &parser.arena.terms[body] else { panic!("expected a label payload") };

    assert_eq!(outer.plain(), "outer");
    assert_eq!(inner.plain(), "inner");
    assert_eq!(payload.plain(), "Int");
}

#[test]
fn annotation_binds_inside_a_named_classifier() {
    let source = "(field :: A : K)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(fields)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized labeled term")
    };
    let [field] = fields.as_slice() else { panic!("expected one labeled term") };
    let Term::Label(Label(name, body)) = &parser.arena.terms[field] else {
        panic!("expected a label")
    };
    let Term::Ann(Ann { tm, ty }) = &parser.arena.terms[body] else {
        panic!("expected the classifier payload to be annotated")
    };
    let Term::Var(payload) = &parser.arena.terms[tm] else {
        panic!("expected a classifier payload")
    };
    let Term::Var(kind) = &parser.arena.terms[ty] else { panic!("expected a kind annotation") };

    assert_eq!(name.plain(), "field");
    assert_eq!(payload.plain(), "A");
    assert_eq!(kind.plain(), "K");
}

#[test]
fn parses_mixed_named_and_labeled_terms_right_associatively() {
    let source = "(outer = inner :: Int)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(fields)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized named term")
    };
    let [field] = fields.as_slice() else { panic!("expected one named term") };
    let Term::Named(Named(outer, body)) = &parser.arena.terms[field] else {
        panic!("expected an outer named term")
    };
    let Term::Label(Label(inner, body)) = &parser.arena.terms[body] else {
        panic!("expected an inner label")
    };
    let Term::Var(payload) = &parser.arena.terms[body] else { panic!("expected a label payload") };

    assert_eq!(outer.plain(), "outer");
    assert_eq!(inner.plain(), "inner");
    assert_eq!(payload.plain(), "Int");
}

#[test]
fn parentheses_classify_the_whole_named_introduction() {
    let source = "((field = value) : (field :: classifier))";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(annotations)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized annotation")
    };
    let [annotation] = annotations.as_slice() else { panic!("expected one annotation") };
    let Term::Ann(Ann { tm, ty }) = &parser.arena.terms[annotation] else {
        panic!("expected an annotation around the named introduction")
    };
    let Term::Paren(Paren(introductions)) = &parser.arena.terms[tm] else {
        panic!("expected a parenthesized named introduction")
    };
    let [introduction] = introductions.as_slice() else {
        panic!("expected one named introduction")
    };
    let Term::Named(Named(introduced, value)) = &parser.arena.terms[introduction] else {
        panic!("expected a named introduction")
    };
    let Term::Var(value) = &parser.arena.terms[value] else {
        panic!("expected an introduced payload")
    };
    let Term::Paren(Paren(classifiers)) = &parser.arena.terms[ty] else {
        panic!("expected a parenthesized named classifier")
    };
    let [classifier] = classifiers.as_slice() else { panic!("expected one named classifier") };
    let Term::Label(Label(classified, classifier)) = &parser.arena.terms[classifier] else {
        panic!("expected a named classifier")
    };
    let Term::Var(classifier) = &parser.arena.terms[classifier] else {
        panic!("expected a classifier payload")
    };

    assert_eq!(introduced.plain(), "field");
    assert_eq!(value.plain(), "value");
    assert_eq!(classified.plain(), "field");
    assert_eq!(classifier.plain(), "classifier");
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
fn parses_punned_named_terms_and_payload_annotations() {
    let source = "(= left, middle, = right : Int)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(fields)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized mixed tuple")
    };
    let [left, middle, right] = fields.as_slice() else {
        panic!("expected three tuple components")
    };

    let Term::Named(Named(left_name, left)) = &parser.arena.terms[left] else {
        panic!("expected a punned left component")
    };
    let Term::Var(left) = &parser.arena.terms[left] else {
        panic!("expected the left payload to be a variable")
    };
    let Term::Var(middle) = &parser.arena.terms[middle] else {
        panic!("expected an unnamed middle component")
    };
    let Term::Named(Named(right_name, right)) = &parser.arena.terms[right] else {
        panic!("expected a punned right component")
    };
    let Term::Ann(Ann { tm: right, ty }) = &parser.arena.terms[right] else {
        panic!("expected the right payload to be annotated")
    };
    let Term::Var(right) = &parser.arena.terms[right] else {
        panic!("expected the right payload to be a variable")
    };
    let Term::Var(ty) = &parser.arena.terms[ty] else { panic!("expected a variable annotation") };

    assert_eq!(left_name.plain(), "left");
    assert_eq!(left.plain(), "left");
    assert_eq!(middle.plain(), "middle");
    assert_eq!(right_name.plain(), "right");
    assert_eq!(right.plain(), "right");
    assert_eq!(ty.plain(), "Int");
}

#[test]
fn field_names_and_puns_accept_uppercase_variable_names() {
    let source = "(Explicit = payload, = Inferred)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Paren(Paren(fields)) = &parser.arena.terms[&term] else {
        panic!("expected a parenthesized named tuple")
    };
    let [explicit, inferred] = fields.as_slice() else { panic!("expected two named components") };
    let Term::Named(Named(explicit_name, explicit)) = &parser.arena.terms[explicit] else {
        panic!("expected an explicit named component")
    };
    let Term::Var(explicit) = &parser.arena.terms[explicit] else {
        panic!("expected an explicit variable payload")
    };
    let Term::Named(Named(inferred_name, inferred)) = &parser.arena.terms[inferred] else {
        panic!("expected a punned named component")
    };
    let Term::Var(inferred) = &parser.arena.terms[inferred] else {
        panic!("expected a punned variable payload")
    };

    assert_eq!(explicit_name.plain(), "Explicit");
    assert_eq!(explicit.plain(), "payload");
    assert_eq!(inferred_name.plain(), "Inferred");
    assert_eq!(inferred.plain(), "Inferred");
}

#[test]
fn rejects_punning_a_non_variable_term() {
    let source = "(= 1)";
    let mut parser = Parser::new();
    let parsed = parser::SingleTermParser::new().parse(
        source,
        &LocationCtx::Plain,
        &mut parser,
        lexer::Lexer::new(source),
    );

    assert!(parsed.is_err());
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
fn parses_punned_named_patterns_and_payload_annotations() {
    let source = "(= left : Int, middle, = right)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &LocationCtx::Plain, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Pattern::Paren(Paren(fields)) = &parser.arena.pats[&pattern] else {
        panic!("expected a parenthesized mixed tuple pattern")
    };
    let [left, middle, right] = fields.as_slice() else {
        panic!("expected three tuple pattern components")
    };

    let Pattern::Named(Named(left_name, left)) = &parser.arena.pats[left] else {
        panic!("expected a punned left pattern")
    };
    let Pattern::Ann(Ann { tm: left, ty }) = &parser.arena.pats[left] else {
        panic!("expected the left payload to be annotated")
    };
    let Pattern::Var(left) = &parser.arena.pats[left] else {
        panic!("expected the left payload to be a variable pattern")
    };
    let Term::Var(ty) = &parser.arena.terms[ty] else { panic!("expected a variable annotation") };
    let Pattern::Var(middle) = &parser.arena.pats[middle] else {
        panic!("expected an unnamed middle pattern")
    };
    let Pattern::Named(Named(right_name, right)) = &parser.arena.pats[right] else {
        panic!("expected a punned right pattern")
    };
    let Pattern::Var(right) = &parser.arena.pats[right] else {
        panic!("expected the right payload to be a variable pattern")
    };

    assert_eq!(left_name.plain(), "left");
    assert_eq!(parser.arena.defs[left].plain(), "left");
    assert_eq!(ty.plain(), "Int");
    assert_eq!(parser.arena.defs[middle].plain(), "middle");
    assert_eq!(right_name.plain(), "right");
    assert_eq!(parser.arena.defs[right].plain(), "right");
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
