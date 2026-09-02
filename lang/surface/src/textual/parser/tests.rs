//! Parser API tests and generated-parser recovery contracts.

mod recovery;

use super::*;

struct RejectionAssertions;

impl RejectionAssertions {
    fn same_issues(failure: &ParseFailure, recovered: &[ParseIssue]) {
        assert_eq!(failure.issue_count(), recovered.len());
        failure.issues().zip(recovered).for_each(|(strict, recovered)| {
            assert_eq!(strict.range, recovered.range);
            assert_eq!(strict.kind, recovered.kind);
            assert_eq!(strict.dropped_tokens, recovered.dropped_tokens);
            // Entity IDs belong to separate arenas; only their hole categories should agree.
            assert_eq!(
                strict.recovery.map(|hole| std::mem::discriminant(&hole.entity)),
                recovered.recovery.map(|hole| std::mem::discriminant(&hole.entity))
            );
        });
    }
}

#[test]
fn expectation_catalog_covers_every_public_grammar_terminal() {
    let grammar = include_str!("grammar.lalrpop");
    let mappings = grammar
        .split_once("enum ParserToken<'input> {")
        .unwrap()
        .1
        .split_once("\n    }")
        .unwrap()
        .0
        .lines()
        .filter_map(|line| line.trim().strip_prefix('"')?.split_once("\" =>").map(|(name, _)| name))
        .filter(|name| !matches!(*name, "Completion" | "Invalid"))
        .collect::<std::collections::BTreeSet<_>>();
    let catalog = SyntaxExpectation::ALL
        .iter()
        .map(|expectation| expectation.parser_name())
        .collect::<std::collections::BTreeSet<_>>();

    assert_eq!(catalog, mappings);
}

#[test]
fn completion_cursor_replaces_the_complete_identifier() {
    let source = "let answer = value in answer";
    let cursor = CompletionCursor::at(source, source.find("lue").unwrap()).unwrap();

    assert_eq!(cursor.replacement(), source.find("value").unwrap()..source.find(" in").unwrap());
}

#[test]
fn completion_cursor_rejects_opaque_source() {
    [
        ("-- hidden\nvalue", 4),
        ("--| documentation\nvalue", 5),
        ("/- outer /- inner -/ comment -/ value", 13),
        ("/- unfinished comment", 5),
        ("\"string\"", 3),
        ("'a'", 1),
    ]
    .into_iter()
    .for_each(|(source, offset)| {
        assert_eq!(
            CompletionCursor::at(source, offset),
            Err(CompletionCursorError::OpaqueSource { offset }),
            "source: {source:?}"
        );
    });
}

#[test]
fn completion_cursor_rejects_offsets_beyond_the_source() {
    [("", 1), ("value", 6), ("é", 3), ("value", usize::MAX)].into_iter().for_each(
        |(source, offset)| {
            assert_eq!(
                CompletionCursor::at(source, offset),
                Err(CompletionCursorError::OutOfBounds { offset, source_len: source.len() })
            );
        },
    );
}

#[test]
fn completion_cursor_rejects_a_non_utf8_boundary() {
    assert_eq!(
        CompletionCursor::at("é", 1),
        Err(CompletionCursorError::InvalidCharacterBoundary { offset: 1 })
    );
}

#[test]
fn completion_cursor_replaces_a_token_that_contains_it() {
    assert_eq!(CompletionCursor::at("123", 1).unwrap().replacement(), 0..3);
}

#[test]
fn recovering_source_creates_a_typed_term_completion_hole() {
    let source = "let value =  in value";
    let offset = source.find(" in value").unwrap();
    let mut parser = Parser::new();
    let parsed = RecoveringParser::at(source, offset).unwrap().source(&mut parser);

    assert!(parsed.syntax.is_some());
    assert!(parsed.issues.is_empty());
    let completion = parsed.completion.expect("the completion site should be retained");
    assert_eq!(completion.replacement, offset..offset);
    assert!(completion.expected.iter().all(|expectation| {
        expectation.source_spelling().is_some() || !expectation.parser_name().is_empty()
    }));
    let CompletionHole { entity: ParsedHole::Term(term) } =
        completion.hole.expect("the cursor should occupy a term hole")
    else {
        panic!("expected a term completion hole")
    };
    assert!(matches!(parser.arena.terms[&term], Term::Hole(_)));

    let mut strict = Parser::new();
    assert!(StrictParser::source(source, &mut strict).is_err());
}

#[test]
fn recovering_source_creates_a_typed_pattern_completion_hole() {
    let source = "fn  => body";
    let offset = source.find(" =>").unwrap();
    let mut parser = Parser::new();
    let parsed = RecoveringParser::at(source, offset).unwrap().source(&mut parser);

    assert!(parsed.syntax.is_some());
    assert!(parsed.issues.is_empty());
    let completion = parsed.completion.expect("the completion site should be retained");
    let CompletionHole { entity: ParsedHole::Pattern(pattern) } =
        completion.hole.expect("the cursor should occupy a pattern hole")
    else {
        panic!("expected a pattern completion hole")
    };
    assert!(matches!(parser.arena.pats[&pattern], Pattern::Hole(_)));
}

#[test]
fn completion_replaces_the_whole_identifier_prefix() {
    let source = "let result = candidate in result";
    let start = source.find("candidate").unwrap();
    let mut parser = Parser::new();
    let parsed = RecoveringParser::at(source, start + 4).unwrap().source(&mut parser);
    let completion = parsed.completion.expect("the completion site should be retained");

    assert!(parsed.syntax.is_some());
    assert!(parsed.issues.is_empty());
    assert_eq!(completion.replacement, start..start + "candidate".len());
    assert!(matches!(completion.hole, Some(CompletionHole { entity: ParsedHole::Term(_) })));
}

#[test]
fn completion_exposes_fixed_term_delimiters_as_typed_expectations() {
    [
        ("let value = body ", SyntaxExpectation::In),
        ("let value = body ", SyntaxExpectation::That),
        ("fn argument ", SyntaxExpectation::TermArrow),
        ("begin value ", SyntaxExpectation::End),
    ]
    .into_iter()
    .for_each(|(source, expected)| {
        let mut parser = Parser::new();
        let parsed = RecoveringParser::at(source, source.len()).unwrap().source(&mut parser);
        let completion = parsed.completion.expect("the completion site should be retained");

        assert!(
            completion.expected.contains(&expected),
            "expected {expected:?} at the end of `{source}`, got {:?}",
            completion.expected
        );
    });
}

#[test]
fn ordinary_recovery_is_typed_and_strict_parsing_rejects_it() {
    let source = "let value = in value";
    let mut parser = Parser::new();
    let parsed = RecoveringParser::new(source).source(&mut parser);

    assert!(parsed.syntax.is_some());
    assert_eq!(parsed.issues.len(), 1);
    assert!(matches!(
        parsed.issues[0].recovery,
        Some(RecoveryHole { entity: ParsedHole::Term(_) })
    ));
    assert!(parsed.completion.is_none());

    let mut strict = Parser::new();
    assert!(StrictParser::source(source, &mut strict).is_err());
}

#[test]
fn strict_source_and_term_reject_syntax_accepted_only_through_recovery() {
    [
        ("let value = in value", "let value = 1 in value"),
        ("fn => body", "fn argument => body"),
        ("let first = (,) in first", "let first = (1,) in first"),
        ("begin end", "begin value end"),
        ("let value = 1 in", "let value = 1 in value"),
    ]
    .into_iter()
    .for_each(|(source, repaired)| {
        let recovered_source = RecoveringParser::new(source).source(&mut Parser::new());
        let recovered_term = RecoveringParser::new(source).term(&mut Parser::new());
        assert!(recovered_source.syntax.is_some(), "source: {source:?}");
        assert!(recovered_term.syntax.is_some(), "source: {source:?}");
        assert!(!recovered_source.issues.is_empty(), "source: {source:?}");
        assert!(recovered_source.issues.iter().all(|issue| issue.recovery.is_some()));

        let source_failure = StrictParser::source(source, &mut Parser::new())
            .expect_err("a recovered source must not pass strict parsing");
        let term_failure = StrictParser::term(source, &mut Parser::new())
            .expect_err("a recovered term must not pass strict parsing");
        RejectionAssertions::same_issues(&source_failure, &recovered_source.issues);
        RejectionAssertions::same_issues(&term_failure, &recovered_term.issues);

        assert!(StrictParser::source(repaired, &mut Parser::new()).is_ok(), "repair: {repaired:?}");
        assert!(StrictParser::term(repaired, &mut Parser::new()).is_ok(), "repair: {repaired:?}");
    });
}

#[test]
fn strict_pattern_rejects_syntax_accepted_only_through_recovery() {
    [("", "_"), ("(,)", "(value,)"), ("+Some", "+Some value")].into_iter().for_each(
        |(source, repaired)| {
            let recovered = RecoveringParser::new(source).pattern(&mut Parser::new());
            assert!(recovered.syntax.is_some(), "source: {source:?}");
            assert!(!recovered.issues.is_empty(), "source: {source:?}");
            assert!(recovered.issues.iter().all(|issue| matches!(
                issue.recovery,
                Some(RecoveryHole { entity: ParsedHole::Pattern(_) })
            )));

            let failure = StrictParser::pattern(source, &mut Parser::new())
                .expect_err("a recovered pattern must not pass strict parsing");
            RejectionAssertions::same_issues(&failure, &recovered.issues);
            assert!(
                StrictParser::pattern(repaired, &mut Parser::new()).is_ok(),
                "repair: {repaired:?}"
            );
        },
    );
}

#[test]
fn strict_parsing_rejects_mismatched_delimiters_and_trailing_tokens() {
    ["(value]", "{value)", "begin value }", "value end", "value => other", "@[format(] value"]
        .into_iter()
        .for_each(|source| {
            let failure = StrictParser::source(source, &mut Parser::new())
                .expect_err("malformed syntax must fail strict parsing");

            assert!(!failure.is_unrecognized_eof(), "source: {source:?}");
            assert!(
                failure.issues().all(|issue| {
                    issue.range.as_ref().is_some_and(|range| {
                        range.start <= range.end && source.get(range.clone()).is_some()
                    })
                }),
                "source: {source:?}"
            );
        });
}

#[test]
fn unknown_tokens_use_grammar_recovery_even_after_a_valid_term() {
    ["?", "value ?", "let value = ? in value"].into_iter().for_each(|source| {
        let recovered = RecoveringParser::new(source).source(&mut Parser::new());
        assert!(recovered.syntax.is_some(), "source: {source:?}");
        assert_eq!(recovered.issues.len(), 1);
        assert!(matches!(
            &recovered.issues[0].kind,
            ParseIssueKind::UnrecognizedToken {
                token: DiagnosticToken::Invalid(LexicalError::UnrecognizedToken),
                ..
            }
        ));
        let offset = source.find('?').unwrap();
        assert_eq!(recovered.issues[0].range, Some(offset..offset + 1));
        assert!(recovered.issues[0].recovery.is_some());
        assert!(recovered.completion.is_none());
        assert!(StrictParser::source(source, &mut Parser::new()).is_err());
    });
}

#[test]
fn a_real_error_followed_by_eof_is_not_just_incomplete_input() {
    let source = "let first = in let second =";
    let failure = StrictParser::source(source, &mut Parser::new()).unwrap_err();

    assert!(!failure.is_unrecognized_eof());
    assert_eq!(failure.issue_count(), 2);
    assert!(matches!(
        &failure.primary().kind,
        ParseIssueKind::UnrecognizedToken { token: DiagnosticToken::Source(token), .. } if token == "in"
    ));
    assert!(failure.issues().last().unwrap().is_unrecognized_eof());
}

#[test]
fn completion_keeps_real_errors_before_and_after_the_cursor() {
    let source = "let first = in let second =  in let third = in third";
    let offset = source.find(" in let third").unwrap();
    let parsed = RecoveringParser::at(source, offset).unwrap().source(&mut Parser::new());

    assert!(parsed.syntax.is_some());
    assert_eq!(parsed.issues.len(), 2);
    let ranges = parsed.issues.iter().map(|issue| issue.range.clone()).collect::<Vec<_>>();
    let first = source.find("in let second").unwrap();
    let third = source.find("in third").unwrap();
    assert_eq!(ranges, [Some(first..first + 2), Some(third..third + 2)]);
    let hole = parsed.completion.unwrap().hole.expect("the cursor should have its own hole");
    assert!(
        parsed
            .issues
            .iter()
            .all(|issue| { issue.recovery.is_some_and(|recovery| recovery.entity != hole.entity) })
    );
}

#[test]
fn completion_does_not_hide_source_tokens_discarded_during_marker_recovery() {
    let source = "let value =  , in value";
    let offset = source.find(" ,").unwrap();
    let parsed = RecoveringParser::at(source, offset).unwrap().source(&mut Parser::new());
    let comma = source.find(',').unwrap();

    assert!(parsed.syntax.is_some());
    assert_eq!(parsed.issues.len(), 1);
    assert_eq!(parsed.issues[0].range, Some(comma..comma + 1));
    assert!(matches!(
        &parsed.issues[0].kind,
        ParseIssueKind::UnrecognizedToken { token: DiagnosticToken::Source(token), .. } if token == ","
    ));
    assert!(parsed.issues[0].dropped_tokens.iter().any(|token| {
        token.range == (comma..comma + 1) && token.token == DiagnosticToken::Source(",".to_owned())
    }));
    assert!(parsed.issues[0].recovery.is_some());
    let completion = parsed.completion.expect("completion facts should survive the source error");
    assert!(completion.expected.contains(&SyntaxExpectation::LowerIdentifier));
}

#[test]
fn ordinary_recovery_does_not_expose_a_discarded_completion_marker_as_source() {
    let source = "let value = ,  in value";
    let offset = source.find(" in value").unwrap();
    let parsed = RecoveringParser::at(source, offset).unwrap().source(&mut Parser::new());
    let comma = source.find(',').unwrap();

    assert!(parsed.syntax.is_some());
    assert_eq!(parsed.issues.len(), 1);
    assert_eq!(parsed.issues[0].range, Some(comma..comma + 1));
    assert_eq!(
        parsed.issues[0].dropped_tokens,
        [DroppedToken { range: comma..comma + 1, token: ",".to_owned().into() }]
    );
}

#[test]
fn nested_recovery_preserves_a_following_complete_binding() {
    let source = "let first = (,) in let second = 2 in second";
    let mut parser = Parser::new();
    let parsed = RecoveringParser::new(source).source(&mut parser);
    let root = parsed.syntax.expect("the enclosing source should recover").root;

    assert!(!parsed.issues.is_empty());
    let Term::ContextBind(first) = &parser.arena.terms[&root] else {
        panic!("expected the recovered outer binding")
    };
    assert!(matches!(parser.arena.terms[&first.binding.bindee], Term::Paren(_)));
    assert!(matches!(parser.arena.terms[&first.tail], Term::ContextBind(_)));
}

#[test]
fn an_authored_hole_is_not_a_completion_or_recovery_hole() {
    let mut parser = Parser::new();
    let parsed = RecoveringParser::new("_").term(&mut parser);
    let term = parsed.syntax.expect("an authored hole should parse");

    assert!(parsed.issues.is_empty());
    assert!(parsed.completion.is_none());
    assert!(matches!(parser.arena.terms[&term], Term::Hole(_)));
    assert_eq!(parser.spans[&EntityId::Term(term)].range(), 0..1);
}
