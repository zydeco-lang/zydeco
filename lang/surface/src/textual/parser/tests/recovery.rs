//! Integration contracts around the generated parser, not a second grammar or repair engine.

use super::{super::*, RejectionAssertions};
use crate::textual::{
    fmt::Formatter,
    syntax::{CoPattern, MetaNode, Span},
    tests::corpus::ZydecoCorpus,
};
use std::{
    collections::{HashMap, HashSet},
    fs,
    mem::{Discriminant, discriminant},
};
use strum::VariantArray;
use zydeco_syntax::Ugly;

#[derive(Debug, Eq, PartialEq)]
enum NodeKind {
    Definition,
    Pattern(Discriminant<Pattern>),
    CoPattern(Discriminant<CoPattern>),
    Metadata(Discriminant<MetaNode>),
    Term(Discriminant<Term>),
}

#[derive(Debug, Eq, PartialEq)]
struct ProjectedNode {
    kind: NodeKind,
    children: Vec<usize>,
}

/// Compare stable AST structure and source-level values without comparing arena IDs.
/// The existing formatter supplies values; tags and edges preserve structure that
/// printing can elide, such as a boundary wrapper or a singleton grouping node.
#[derive(Debug, Eq, PartialEq)]
struct SyntaxProjection {
    rendered: String,
    nodes: Vec<ProjectedNode>,
}

impl SyntaxProjection {
    fn with_root(parser: &Parser, root: EntityId) -> (Self, Vec<Range<usize>>) {
        let arena = &parser.arena;
        let formatter = Formatter::new(arena);
        let rendered = match root {
            | EntityId::Def(id) => id.ugly(&formatter),
            | EntityId::Pat(id) => id.ugly(&formatter),
            | EntityId::CoPat(id) => id.ugly(&formatter),
            | EntityId::Meta(id) => id.ugly(&formatter),
            | EntityId::Term(id) => id.ugly(&formatter),
        };
        let mut indices = HashMap::new();
        let mut order = Vec::new();
        let mut pending = vec![root];
        while let Some(entity) = pending.pop() {
            if let std::collections::hash_map::Entry::Vacant(entry) = indices.entry(entity) {
                entry.insert(order.len());
                order.push(entity);
                pending.extend(arena.children(entity).into_iter().rev());
            }
        }
        let nodes = order
            .iter()
            .map(|entity| {
                let kind = match entity {
                    | EntityId::Def(_) => NodeKind::Definition,
                    | EntityId::Pat(id) => NodeKind::Pattern(discriminant(&arena.pats[id])),
                    | EntityId::CoPat(id) => NodeKind::CoPattern(discriminant(&arena.copats[id])),
                    | EntityId::Meta(id) => NodeKind::Metadata(discriminant(&arena.metas[id])),
                    | EntityId::Term(id) => NodeKind::Term(discriminant(&arena.terms[id])),
                };
                ProjectedNode {
                    kind,
                    children: arena.children(*entity).iter().map(|child| indices[child]).collect(),
                }
            })
            .collect();
        let ranges = order.iter().map(|entity| parser.spans[entity].range()).collect();
        (Self { rendered, nodes }, ranges)
    }
}

#[derive(Copy, Clone, Debug, strum::VariantArray)]
enum EntryPoint {
    Source,
    Term,
    Pattern,
}

impl EntryPoint {
    fn strict(self, source: &str, parser: &mut Parser) -> Result<EntityId, ParseFailure> {
        match self {
            | Self::Source => StrictParser::source(source, parser).map(|root| root.entity()),
            | Self::Term => StrictParser::term(source, parser).map(|root| root.entity()),
            | Self::Pattern => StrictParser::pattern(source, parser).map(|root| root.entity()),
        }
    }

    fn recover(
        self, request: &RecoveringParser<'_>, parser: &mut Parser,
    ) -> RecoveringParse<EntityId> {
        match self {
            | Self::Source => Self::erase(request.source(parser)),
            | Self::Term => Self::erase(request.term(parser)),
            | Self::Pattern => Self::erase(request.pattern(parser)),
        }
    }

    fn erase<Root: ParsedRoot>(parsed: RecoveringParse<Root>) -> RecoveringParse<EntityId> {
        let RecoveringParse { syntax, issues, completion } = parsed;
        RecoveringParse { syntax: syntax.map(|root| root.entity()), issues, completion }
    }
}

struct ParserContract {
    entry: EntryPoint,
}

impl ParserContract {
    fn range(source: &str, range: &Range<usize>) {
        assert!(
            range.start <= range.end && source.get(range.clone()).is_some(),
            "{range:?} in {source:?}"
        );
    }

    fn hole(parser: &Parser, hole: ParsedHole) {
        match hole {
            | ParsedHole::Pattern(id) => {
                assert!(matches!(parser.arena.pats[&id], Pattern::Hole(_)))
            }
            | ParsedHole::Term(id) => assert!(matches!(parser.arena.terms[&id], Term::Hole(_))),
        }
    }

    fn invariants(source: &str, parser: &Parser, parsed: &RecoveringParse<EntityId>) {
        parser.spans.iter().for_each(|(_, span)| Self::range(source, &span.range()));
        assert!(
            parsed.syntax.is_some() || !parsed.issues.is_empty() || parsed.completion.is_some()
        );
        let mut holes = HashSet::new();
        parsed.issues.iter().for_each(|issue| {
            Self::range(source, issue.range.as_ref().expect("source errors must carry ranges"));
            if let Some(hole) = issue.recovery {
                Self::hole(parser, hole.entity);
                assert!(
                    holes.insert(hole.entity.entity()),
                    "two recovery actions share an allocated hole"
                );
            }
            issue.dropped_tokens.iter().for_each(|token| {
                Self::range(source, &token.range);
                assert_ne!(token.token, DiagnosticToken::Source("<completion>".to_owned()));
            });
            assert!(
                issue
                    .dropped_tokens
                    .windows(2)
                    .all(|pair| pair[0].range.end <= pair[1].range.start)
            );
        });
        let reachable =
            parsed.syntax.map(|root| parser.arena.reachable_from(root)).unwrap_or_default();
        if let Some(completion) = &parsed.completion {
            Self::range(source, &completion.replacement);
            if let Some(hole) = completion.hole {
                Self::hole(parser, hole.entity);
                assert!(
                    reachable.contains(&hole.entity.entity()),
                    "completion exposes an abandoned node"
                );
            }
            assert!(completion.expected.iter().all(|expected| expected.parser_name().is_some()));
        }
    }

    /// The strict LALRPOP facade is the reference for acceptance. Separate arena IDs
    /// are erased, while reachable shape, values, and spans must agree on success.
    fn check(&self, source: &str) {
        let mut parser = Parser::new();
        let recovered = self.entry.recover(&RecoveringParser::new(source), &mut parser);
        Self::invariants(source, &parser, &recovered);
        let mut reference = Parser::new();
        match self.entry.strict(source, &mut reference) {
            | Ok(root) => {
                assert!(recovered.issues.is_empty(), "{:?}: {source:?}", self.entry);
                assert_eq!(
                    SyntaxProjection::with_root(&reference, root),
                    SyntaxProjection::with_root(&parser, recovered.syntax.unwrap()),
                    "{:?}: {source:?}",
                    self.entry,
                );
            }
            | Err(failure) => RejectionAssertions::same_issues(&failure, &recovered.issues),
        }
        // Recovery choices must also be deterministic on rejected input.
        let mut repeated_parser = Parser::new();
        let repeated = self.entry.recover(&RecoveringParser::new(source), &mut repeated_parser);
        assert_eq!(
            recovered.syntax.map(|root| SyntaxProjection::with_root(&parser, root)),
            repeated.syntax.map(|root| SyntaxProjection::with_root(&repeated_parser, root)),
            "{:?}: {source:?}",
            self.entry,
        );
        assert_eq!(recovered.issues.len(), repeated.issues.len());
        if !repeated.issues.is_empty() {
            RejectionAssertions::same_issues(
                &ParseFailure::new(repeated.issues),
                &recovered.issues,
            );
        }
    }

    fn completion(&self, source: &str, offset: usize) {
        let request = match RecoveringParser::at(source, offset) {
            | Ok(request) => request,
            | Err(CompletionCursorError::OpaqueSource { .. }) => return,
            | Err(error) => panic!("invalid test cursor: {error}"),
        };
        let mut parser = Parser::new();
        let parsed = self.entry.recover(&request, &mut parser);
        Self::invariants(source, &parser, &parsed);
        let completion = parsed.completion.unwrap();
        assert!(completion.replacement.start <= offset && offset <= completion.replacement.end);
    }

    fn repair(&self, source: &str, repaired: &str) {
        let mut parser = Parser::new();
        let recovered = self.entry.recover(&RecoveringParser::new(source), &mut parser);
        Self::invariants(source, &parser, &recovered);
        assert!(!recovered.issues.is_empty(), "a repair fixture must start malformed: {source:?}");
        assert!(self.entry.strict(source, &mut Parser::new()).is_err());
        let mut reference = Parser::new();
        let repaired_root = self
            .entry
            .strict(repaired, &mut reference)
            .unwrap_or_else(|error| panic!("bad repair {repaired:?}: {error:?}"));
        assert_eq!(
            SyntaxProjection::with_root(&parser, recovered.syntax.unwrap()).0,
            SyntaxProjection::with_root(&reference, repaired_root).0,
            "recovery of {source:?} differs from explicit repair {repaired:?}",
        );
    }
}

#[test]
fn repository_sources_agree_with_strict_parsing_in_shape_values_and_spans() {
    ZydecoCorpus::files().into_iter().for_each(|path| {
        let source = fs::read_to_string(&path).unwrap();
        assert!(StrictParser::source(&source, &mut Parser::new()).is_ok(), "{}", path.display());
        ParserContract { entry: EntryPoint::Source }.check(&source);
        ParserContract { entry: EntryPoint::Term }.check(&source);
    });
}

#[test]
fn recovery_matches_explicit_strict_repairs_and_retains_later_syntax() {
    [
        ("let value = in value", "let value = _ in value"),
        (
            "let first = (,) in let second = 2 in second",
            "let first = (_,) in let second = 2 in second",
        ),
        ("fn => body", "fn _ => body"),
        ("match value | +A => 1 | +B b => b end", "match value | +A _ => 1 | +B b => b end"),
        ("comatch | .get => | .next x => x end", "comatch | .get => _ | .next x => x end"),
        ("@[tag(x)] let value = ? in value", "@[tag(x)] let value = _ in value"),
    ]
    .into_iter()
    .for_each(|(source, repaired)| {
        ParserContract { entry: EntryPoint::Source }.repair(source, repaired);
        ParserContract { entry: EntryPoint::Term }.repair(source, repaired);
    });
    [("", "_"), ("(,)", "(_, )"), ("+Some", "+Some _")].into_iter().for_each(
        |(source, repaired)| {
            ParserContract { entry: EntryPoint::Pattern }.repair(source, repaired);
        },
    );
}

#[test]
fn completion_matches_an_explicit_hole_at_the_validated_replacement() {
    [
        "let value = <cursor> in value",
        "fn <cursor> => body",
        "fn argument => <cursor>",
        "let value = pre<cursor>fix in value",
        "let value = val<cursor> in value",
        "let value = _<cursor> in value",
        "match value | +A <cursor> => 1 | +B b => b end",
        "@[tag(\"🦀\")] let value = <cursor> in value",
    ]
    .into_iter()
    .for_each(|marked| {
        let offset = marked.find("<cursor>").unwrap();
        let source = marked.replacen("<cursor>", "", 1);
        let mut parser = Parser::new();
        let request = RecoveringParser::at(&source, offset).unwrap();
        let parsed = request.source(&mut parser);
        assert!(parsed.issues.is_empty(), "{marked:?}: {:?}", parsed.issues);
        let completion = parsed.completion.unwrap();
        let hole = completion.hole.expect("a semantic cursor needs an allocated hole");
        let root = parsed.syntax.unwrap().root;
        assert!(parser.arena.reachable_from(root.into()).contains(&hole.entity.entity()));
        assert_eq!(parser.spans[&hole.entity.entity()].range(), offset..offset);
        let mut repaired = source.clone();
        repaired.replace_range(completion.replacement, "_");
        let mut reference = Parser::new();
        let repaired_root = StrictParser::source(&repaired, &mut reference).unwrap().root;
        assert_eq!(
            SyntaxProjection::with_root(&parser, root.into()).0,
            SyntaxProjection::with_root(&reference, repaired_root.into()).0,
            "{marked:?}",
        );
    });
}

#[test]
fn completion_cursor_retains_word_roles_and_only_the_typed_prefix() {
    for (source, offset, prefix, kind) in [
        ("value", 3, "val", LexicalTokenKind::LowerIdentifier),
        ("val", 3, "val", LexicalTokenKind::Keyword),
        ("_", 1, "_", LexicalTokenKind::Hole),
        (".field", 3, ".fi", LexicalTokenKind::Destructor),
    ] {
        let cursor = CompletionCursor::at(source, offset).unwrap();
        assert_eq!(cursor.replacement(), 0..source.len());
        assert_eq!(cursor.prefix(), prefix);
        assert_eq!(cursor.token_kind(), Some(kind));
    }
    let cursor = CompletionCursor::at("val ", 4).unwrap();
    assert_eq!(cursor.replacement(), 4..4);
    assert_eq!(cursor.prefix(), "");
    assert_eq!(cursor.token_kind(), None);
}

#[test]
fn direct_recovery_handles_survive_equal_spans_and_out_of_order_allocation() {
    let mut parser = Parser::new();
    let mut recovery = RecoveryAccumulator::default();
    let first: ParsedNode<Term> = recovery.recover(
        0..0,
        ErrorRecovery {
            error: LalrpopParseError::UnrecognizedEof { location: 0, expected: vec![] },
            dropped_tokens: vec![],
        },
    );
    let second: ParsedNode<Term> = recovery.recover(
        0..0,
        ErrorRecovery {
            error: LalrpopParseError::UnrecognizedEof { location: 0, expected: vec![] },
            dropped_tokens: vec![],
        },
    );
    let _abandoned: ParsedNode<Pattern> = recovery.recover(
        0..0,
        ErrorRecovery {
            error: LalrpopParseError::UnrecognizedEof { location: 0, expected: vec![] },
            dropped_tokens: vec![],
        },
    );
    // Authored and abandoned allocations deliberately collide in source position.
    let authored = parser.term(Span::new(0, 0).make(Term::Hole(Hole)));
    let second_id = recovery.alloc_term(&mut parser, Span::new(0, 0).make(second));
    let first_id = recovery.alloc_term(&mut parser, Span::new(0, 0).make(first));
    let issues = recovery.finish();
    assert_eq!(issues[0].recovery.unwrap().entity, ParsedHole::Term(first_id));
    assert_eq!(issues[1].recovery.unwrap().entity, ParsedHole::Term(second_id));
    assert!(issues[2].recovery.is_none(), "a discarded semantic value never acquired an AST ID");
    assert!(
        issues.iter().all(|issue| issue
            .recovery
            .is_none_or(|hole| hole.entity != ParsedHole::Term(authored)))
    );
}

#[test]
fn fatal_literal_failure_does_not_expose_an_already_allocated_completion_hole() {
    let source = "let value =  in @[tag(9223372036854775808)] value";
    let offset = source.find(" in").unwrap();
    let mut parser = Parser::new();
    let parsed = RecoveringParser::at(source, offset).unwrap().source(&mut parser);
    assert!(parsed.syntax.is_none());
    assert!(parser.arena.terms.iter().any(|(_, term)| matches!(term, Term::Hole(_))));
    assert!(parsed.completion.unwrap().hole.is_none());
    assert!(matches!(parsed.issues.last().unwrap().kind, ParseIssueKind::Literal { .. }));
}

#[test]
fn completion_does_not_expose_a_hole_popped_by_later_recovery() {
    let source = "( ,) in";
    let mut parser = Parser::new();
    let parsed = RecoveringParser::at(source, 1).unwrap().source(&mut parser);
    let root = parsed.syntax.expect("later damage should still recover a root").root;
    let reachable = parser.arena.reachable_from(root.into());
    assert!(
        parser.arena.terms.iter().any(|(id, term)| {
            matches!(term, Term::Hole(_)) && !reachable.contains(&EntityId::Term(*id))
        }),
        "the fixture must actually abandon a hole"
    );
    assert!(parsed.completion.unwrap().hole.is_none());
    assert!(!parsed.issues.is_empty());
}

#[test]
fn a_source_bound_cursor_stays_valid_across_repeated_parses_and_earlier_allocations() {
    let request = RecoveringParser::with_completion(
        CompletionCursor::at("let value =  in value", 12).unwrap(),
    );
    let mut parser = Parser::new();
    let old = StrictParser::term("_", &mut parser).unwrap();
    let first = request.source(&mut parser).completion.unwrap().hole.unwrap();
    let second = request.source(&mut parser).completion.unwrap().hole.unwrap();
    assert_ne!(first.entity, ParsedHole::Term(old));
    assert_ne!(first.entity, second.entity);
}

#[test]
fn literal_overflow_is_a_typed_fatal_error_with_the_numeric_span() {
    ["9223372036854775808", "-9223372036854775809"].into_iter().for_each(|number| {
        let source = format!("@[tag({number})] value");
        let mut parser = Parser::new();
        let recovered = RecoveringParser::new(&source).source(&mut parser);
        assert!(recovered.syntax.is_none());
        assert_eq!(recovered.issues.len(), 1);
        let issue = &recovered.issues[0];
        assert_eq!(issue.range, Some(6..6 + number.len()));
        assert!(matches!(
            issue.kind,
            ParseIssueKind::Literal { error: LiteralError::MetadataInteger(_) }
        ));
        assert!(issue.recovery.is_none());
        let failure = StrictParser::source(&source, &mut Parser::new()).unwrap_err();
        RejectionAssertions::same_issues(&failure, &recovered.issues);
        // Ordinary integer literals are arbitrary precision, not metadata integers.
        assert!(StrictParser::term(number, &mut Parser::new()).is_ok());
    });
    ["9223372036854775807", "-9223372036854775808"].into_iter().for_each(|number| {
        assert!(
            StrictParser::source(&format!("@[tag({number})] value"), &mut Parser::new()).is_ok()
        );
    });
}

#[test]
fn recovery_issues_before_a_fatal_literal_failure_remain_visible() {
    let source = "let first = in @[tag(9223372036854775808)] first";
    let mut parser = Parser::new();
    let parsed = RecoveringParser::new(source).source(&mut parser);
    assert!(parsed.syntax.is_none());
    assert_eq!(parsed.issues.len(), 2);
    assert!(parsed.issues[0].recovery.is_some());
    assert!(matches!(parsed.issues[1].kind, ParseIssueKind::Literal { .. }));
    let failure = StrictParser::source(source, &mut Parser::new()).unwrap_err();
    RejectionAssertions::same_issues(&failure, &parsed.issues);
}

#[test]
fn malformed_lexemes_recover_without_silent_eof_and_pair_with_valid_sources() {
    [
        ("value -/ tail", "value /- comment -/ tail", LexicalError::UnexpectedCommentClose),
        ("value /- unfinished", "value /- finished -/", LexicalError::UnterminatedBlockComment),
        ("\"unfinished", "\"finished\"", LexicalError::UnterminatedString),
        ("'unfinished", "'x'", LexicalError::UnterminatedCharacter),
        ("'ab'", "'a'", LexicalError::InvalidCharacter),
        ("let value = 🦀 in value", "let value = 0 in value", LexicalError::UnrecognizedToken),
        ("value \0 tail", "value tail", LexicalError::UnrecognizedToken),
    ].into_iter().for_each(|(source, valid, error)| {
        let failure = StrictParser::source(source, &mut Parser::new()).unwrap_err();
        assert!(failure.issues().any(|issue| matches!(
            &issue.kind, ParseIssueKind::UnrecognizedToken { token: DiagnosticToken::Invalid(actual), .. } if *actual == error
        )), "{source:?}: {failure:?}");
        assert!(StrictParser::source(valid, &mut Parser::new()).is_ok(), "{valid:?}");
        ParserContract { entry: EntryPoint::Source }.check(source);
    });
}

#[test]
fn incomplete_opaque_tokens_own_their_eof_cursor_but_closed_tokens_do_not() {
    ["-- comment", "--| documentation", "/- comment", "\"unfinished", "'unfinished"]
        .into_iter()
        .for_each(|source| {
            assert_eq!(
                CompletionCursor::at(source, source.len()),
                Err(CompletionCursorError::OpaqueSource { offset: source.len() })
            );
        });
    ["-- comment\n", "/- comment -/", "\"finished\"", "'x'"].into_iter().for_each(|source| {
        assert!(CompletionCursor::at(source, source.len()).is_ok(), "{source:?}");
    });
}

#[test]
fn completion_retains_lexical_damage_discarded_with_its_marker() {
    let source = "let value =  ? in value";
    let offset = source.find(" ?").unwrap();
    let parsed = RecoveringParser::at(source, offset).unwrap().source(&mut Parser::new());
    assert!(parsed.syntax.is_some());
    assert!(matches!(
        parsed.issues[0].kind,
        ParseIssueKind::UnrecognizedToken {
            token: DiagnosticToken::Invalid(LexicalError::UnrecognizedToken),
            ..
        }
    ));
    assert_eq!(parsed.issues[0].range, Some(offset + 1..offset + 2));
}

#[test]
fn deterministic_token_mutations_and_edit_prefixes_obey_the_parser_contract() {
    [
        "begin let x = 1 in let y = (x, 2) in fn arg => (y, arg) end",
        "match value | +A a => a | +B b => b end",
        "comatch | .get => ret 1 | .put value => ret value end",
        "@[tag(\"🦀\", nested(1))] exists (X as A : VType) . X",
        "pack (X as A : VType) where (#field = value), value end",
        "fn (left; right) => value /field |> function",
        "/- outer /- inner -/ tail -/ let value = 'x' in value",
        r#"@[tag("a\"b\\c")] let value' = '\n' in value'"#,
        "data | +A : A * B | +B : B end",
        "codata | .get x : Ret A | .put y : Ret A end",
        "val pi (x : A) . A -> B",
        "do x <- ret 1; fix rest => { ! rest }",
        "let value = +1.25e-2 in\r\nvalue",
    ]
    .into_iter()
    .for_each(|source| {
        assert!(StrictParser::source(source, &mut Parser::new()).is_ok(), "bad seed: {source:?}");
        EntryPoint::VARIANTS.iter().copied().for_each(|entry| {
            let contract = ParserContract { entry };
            // An editor observes every prefix while typing, including inside UTF-8 literals.
            source.char_indices().map(|(offset, _)| offset).chain([source.len()]).for_each(
                |offset| {
                    contract.check(&source[..offset]);
                    contract.completion(&source[..offset], offset);
                    contract.completion(source, offset);
                },
            );
            Lexer::new(source).map(Result::unwrap).for_each(|(start, _, end)| {
                ["", "?", "]"].into_iter().for_each(|replacement| {
                    let mut edited = source.to_owned();
                    edited.replace_range(start..end, replacement);
                    contract.check(&edited);
                    contract.completion(&edited, start);
                });
            });
        });
    });
}
