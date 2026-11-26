use super::*;
use crate::{FormattedSource, IndentWidth};
use zydeco_surface::{
    scoped::{ResolveFolder, fmt::Formatter as ScopedFormatter},
    textual::{LexicalRole, LexicalTokens, StrictParser},
};
use zydeco_syntax::Ugly;

struct ParsedSource {
    formatted: FormattedSource,
    source: String,
}

struct RetainedComments;

impl RetainedComments {
    fn collect(source: &str) -> Vec<(LexicalRole, String)> {
        LexicalTokens::new(source)
            .filter(|token| matches!(token.role, LexicalRole::Comment | LexicalRole::TextBlock))
            .map(|token| {
                let comment = &source[token.range];
                let comment = comment.strip_suffix('\n').unwrap_or(comment);
                let comment = comment.strip_suffix('\r').unwrap_or(comment);
                (token.role, comment.to_string())
            })
            .collect()
    }
}

impl ParsedSource {
    fn new(source: &str) -> Self {
        Self::named(source, "test source")
    }

    fn named(source: &str, name: &str) -> Self {
        let formatted = FormattedSource::parse(source)
            .unwrap_or_else(|error| panic!("failed to parse {name}: {error:?}\n{source}"));
        Self { formatted, source: source.to_owned() }
    }

    fn render(&self, layout_intentions: LayoutIntentions) -> String {
        self.render_with_options(
            PrettyOptions::default().with_line_width(120).with_layout_intentions(layout_intentions),
        )
    }

    fn render_with_options(&self, options: PrettyOptions) -> String {
        self.formatted.render(&self.source, options)
    }

    fn desugared_shape(&self) -> String {
        let output = ResolveFolder::new(&self.formatted.spans, &self.formatted.arena)
            .run_open_fragment(self.formatted.unit.root)
            .unwrap();
        output.root.ugly(&ScopedFormatter::alpha_normalized(&output.arena))
    }
}

#[test]
fn records_layout_intentions_without_adding_syntax_variants() {
    let source = "(#field = field,\n= kept)";
    let parsed = ParsedSource::new(source);
    assert_eq!(
        parsed.formatted.presentation.intentions.line_extent(parsed.formatted.unit.root.into()),
        Some(LineExtent::new(0, 1))
    );
    let Term::Paren(Paren(fields)) = &parsed.formatted.arena.terms[&parsed.formatted.unit.root]
    else {
        panic!("expected a tuple")
    };
    assert_eq!(fields.len(), 2);
    assert!(
        fields.iter().all(|field| matches!(parsed.formatted.arena.terms[field], Term::Named(_)))
    );
}

#[test]
fn canonicalizes_term_and_pattern_puns_independently() {
    let source = concat!(
        "let (#field = field, /projected = projected) = input in ",
        "(#field = field, = kept, #annotated = annotated : Type, #renamed = other)",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);
    assert_eq!(
        formatted,
        concat!(
            "let (= field, /projected) = input in\n",
            "(= field, = kept, = annotated : Type, #renamed = other)\n",
        )
    );
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
}

#[test]
fn punned_annotations_use_the_surviving_classifier_boundary() {
    let cases = [
        ("(#field =\nfield : Type)", "(= field : Type)\n"),
        ("param (#field =\nfield : Type) that _", "param = field : Type that\n_\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    });
}

#[test]
fn preserves_annotation_ownership_around_punned_fields() {
    let cases = [
        ("param (/process) : Type that _", "param (/process) : Type that\n_\n"),
        ("((#field = field) : Type)", "((= field) : Type)\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn spaces_inline_thunk_contents_and_indents_broken_contents() {
    let cases = [
        ("{SomeComputation}", "{ SomeComputation }\n"),
        ("{ SomeComputation }", "{ SomeComputation }\n"),
        ("{\nSomeComputation\n}", "{\n  SomeComputation\n}\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert!(formatted.lines().all(|line| line.trim_end() == line));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });

    let parsed = ParsedSource::new("{SomeComputation}");
    assert_eq!(
        parsed.render_with_options(
            PrettyOptions::default()
                .with_line_width(12)
                .with_layout_intentions(LayoutIntentions::Ignore),
        ),
        "{\n  SomeComputation\n}\n"
    );
}

#[test]
fn escaped_literals_preserve_their_values_when_formatted() {
    for source in [r#""a\0b\u{1F600}\u{7}""#, r"'\0'", r"'\u{1f600}'", r"'\u{10ffff}'"] {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(reparsed.render(LayoutIntentions::Preserve), formatted);
    }
}

#[test]
fn deeply_nested_groups_share_layout_documents() {
    let depth = 128;
    let grouped = format!("{}0{}", "(".repeat(depth), ")".repeat(depth));
    for (source, expected) in [
        (grouped.clone(), "0\n"),
        (format!("match input | {grouped} => ret 0 end"), "match input\n| 0 => ret 0\nend\n"),
    ] {
        let parsed = ParsedSource::new(&source);
        let formatter = PrettyFormatter::with_source(
            &parsed.formatted.arena,
            &parsed.formatted.spans,
            &parsed.formatted.presentation,
            &parsed.source,
        );
        assert_eq!(formatter.render_unit(parsed.formatted.unit), expected);
        assert!(formatter.documents.terms.borrow().len() <= 3 * (depth + 10));
        assert!(formatter.documents.patterns.borrow().len() <= 2 * (depth + 1));
    }
}

#[test]
fn removes_only_parentheses_allowed_by_the_grammar_context() {
    let cases = [
        ("((x))", "x\n"),
        ("((A -> B))", "A -> B\n"),
        ("(A -> B) -> C", "(A -> B) -> C\n"),
        ("A -> (B -> C)", "A -> B -> C\n"),
        ("(A * B) * C", "(A * B) * C\n"),
        ("A * (B * C)", "A * (B * C)\n"),
        ("! (value/field)", "! value/field\n"),
        ("ret (value/field)", "ret value/field\n"),
        ("+Some value/field", "+Some(value/field)\n"),
        ("(! value)/field", "(! value)/field\n"),
        ("(ret value)/field", "(ret value)/field\n"),
        ("(+Some(value))/field", "(+Some(value))/field\n"),
        ("f (value/field)", "f value/field\n"),
        ("f (! value/field)", "f ! value/field\n"),
        ("f (g x)", "f (g x)\n"),
        ("(f x)/field", "(f x)/field\n"),
        ("((#field = field))", "(= field)\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        assert_eq!(parsed.render(LayoutIntentions::Ignore), expected, "source: {source}");
        let reparsed = ParsedSource::new(expected);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });

    let parsed = ParsedSource::new("((x))");
    assert_eq!(
        parsed.render_with_options(
            PrettyOptions::default()
                .with_line_width(120)
                .with_layout_intentions(LayoutIntentions::Ignore)
                .with_parentheses(Parentheses::Preserve),
        ),
        "((x))\n"
    );
}

#[test]
fn does_not_duplicate_constructor_pattern_parentheses() {
    let canonical = concat!(
        "match tree\n",
        "| +Leaf() => ret +Leaf()\n",
        "| +Node(/left; /value; /right) => ret +Leaf()\n",
        "end\n",
    );
    let redundant = concat!(
        "match tree\n",
        "| +Leaf() => ret +Leaf()\n",
        "| +Node((/left; /value; /right)) => ret +Leaf()\n",
        "end\n",
    );

    [canonical, redundant].into_iter().for_each(|source| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, canonical, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn does_not_add_parentheses_to_multiline_applications() {
    let canonical = concat!("Thk\n", "  (\n", "    forall (B : CType) .\n", "      B\n", "  )\n",);
    let parsed = ParsedSource::new(canonical);
    let formatted = parsed.render_with_options(PrettyOptions::default());

    assert_eq!(formatted, canonical);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());

    let redundant = concat!(
        "(\n",
        "  (\n",
        "    Thk\n",
        "    (\n",
        "      forall (B : CType) .\n",
        "        B\n",
        "    )\n",
        "  )\n",
        ")\n",
    );
    let redundant = ParsedSource::new(redundant);
    assert_eq!(redundant.render_with_options(PrettyOptions::default()), canonical);
    assert_eq!(redundant.desugared_shape(), parsed.desugared_shape());
}

#[test]
fn keeps_a_joined_application_boundary_around_a_multiline_argument() {
    let source = concat!("Thk (\n", "  forall (B : CType) .\n", "    B\n", ")\n",);
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render_with_options(PrettyOptions::default());

    assert_eq!(formatted, source);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn keeps_right_nested_terms_at_the_same_precedence_unparenthesized() {
    let cases = [
        (
            "begin let first = value that let second = first that second end",
            concat!(
                "begin\n",
                "  let first = value that\n",
                "  let second = first that\n",
                "  second\n",
                "end\n",
            ),
        ),
        (
            "forall (A : Type) . forall (B : Type) . A -> B",
            "forall (A : Type) . forall (B : Type) . A -> B\n",
        ),
        ("fn x => fn y => x", "fn x => fn y => x\n"),
        (
            "do x <- first; do y <- second; ret y",
            concat!("do x <- first;\n", "do y <- second;\n", "ret y\n"),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, expected, "source: {source}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn backward_application_omits_right_operand_parentheses() {
    let cases = [
        ("apply <| (fn x => x)", "apply <| fn x => x\n"),
        ("apply <| (val x => x)", "apply <| val x => x\n"),
        ("Thk <| (forall (A : VType) . A -> Ret A)", "Thk <| forall (A : VType) . A -> Ret A\n"),
        ("first <| (second <| value)", "first <| second <| value\n"),
        ("(first <| second) <| value", "(first <| second) <| value\n"),
        ("(apply <| fn x => x) argument", "(apply <| fn x => x) argument\n"),
    ];
    for (source, expected) in cases {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, expected, "source: {source}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }

    let multiline =
        concat!("WithMonad <| fn (M : VType -> CType) =>\n", "  IState S M * IException E M\n",);
    let parsed = ParsedSource::new(multiline);
    let formatted = parsed.render(LayoutIntentions::Preserve);
    assert_eq!(formatted, multiline);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn aligns_sequential_binders_at_the_current_indentation() {
    let cases = [
        (
            "let first = one in let second = two in second",
            concat!("let first = one in\n", "let second = two in\n", "second\n"),
        ),
        (
            "let first = one that let second = two that second",
            concat!("let first = one that\n", "let second = two that\n", "second\n"),
        ),
        (
            "def first = one in def second = two in second",
            concat!("def first = one in\n", "def second = two in\n", "second\n"),
        ),
        (
            "def first = one that def second = two that second",
            concat!("def first = one that\n", "def second = two that\n", "second\n"),
        ),
        (
            "param (A : VType) in param (B : VType) in body",
            concat!("param A : VType in\n", "param B : VType in\n", "body\n"),
        ),
        (
            "param (A : VType) that param (B : VType) that body",
            concat!("param A : VType that\n", "param B : VType that\n", "body\n"),
        ),
        (
            "param val (A : VType) in param val (value : A) in value",
            concat!("param val A : VType in\n", "param val value : A in\n", "value\n"),
        ),
        (
            "param val (A : VType) that param val (value : A) that value",
            concat!("param val A : VType that\n", "param val value : A that\n", "value\n",),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn preserves_at_most_one_empty_line_between_sequential_expressions() {
    let cases = [
        ("let a = 1 in\na", "let a = 1 in\na\n"),
        ("let a = 1 in\n\na", "let a = 1 in\n\na\n"),
        ("let a = 1 in\n\n\na", "let a = 1 in\n\na\n"),
        ("let a = 1 in\n \t \na", "let a = 1 in\n\na\n"),
        ("do a <- first;\n\n\nsecond", "do a <- first;\n\nsecond\n"),
        ("def a = first in\n\nsecond", "def a = first in\n\nsecond\n"),
        ("param (A : VType) that\n\nbody", "param A : VType that\n\nbody\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert!(formatted.lines().all(|line| line.trim_end() == line));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });

    let parsed = ParsedSource::new("let a = 1 in\n\n\na");
    assert_eq!(parsed.render(LayoutIntentions::Ignore), "let a = 1 in\na\n");
}

#[test]
fn keeps_empty_line_intentions_local_to_each_parsed_source() {
    let first = concat!("begin\n", "  let a = 1 in\n", "\n", "\n", "  a\n", "end");
    let second = concat!("begin\n", "  let b =\n", "    value\n", "  in\n", "  b\n", "end");
    let mut parser = Parser::for_formatting();
    let mut presentation = FormatPresentation::default();
    StrictParser::source(first, &mut parser).expect("the first source should parse");
    presentation.capture(first, &parser);
    let second_unit =
        StrictParser::source(second, &mut parser).expect("the second source should parse");
    presentation.capture(second, &parser);

    let formatted = PrettyFormatter::with_options(
        &parser.arena,
        &parser.spans,
        &presentation,
        PrettyOptions::default().with_line_width(120),
    )
    .render_unit(second_unit);

    assert_eq!(formatted, format!("{second}\n"));
}

#[test]
fn preserves_the_position_of_an_empty_line_around_a_leading_comment() {
    let cases = [
        ("let a = 1 in\n-- Tail note.\n\na", "let a = 1 in\n-- Tail note.\n\na\n"),
        ("let a = 1 in\n\n-- Tail note.\na", "let a = 1 in\n\n-- Tail note.\na\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn preserves_one_empty_line_at_composed_layout_boundaries() {
    let cases = [
        ("(first,\n\n\nsecond)", "(first,\n\n  second)\n"),
        ("fn x =>\n\n\nbody", "fn x =>\n\n  body\n"),
        ("A *\n\n\nB", "  A\n\n* B\n"),
        ("{\n\n\nbody\n\n\n}", "{\n\n  body\n\n}\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert!(formatted.lines().all(|line| line.trim_end() == line));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn leads_multiline_infix_operators_without_recursive_indentation() {
    let cases = [
        ("A *\nB *\nC", concat!("  A\n", "* B\n", "* C\n")),
        ("A ->\nB ->\nC", concat!("   A\n", "-> B\n", "-> C\n")),
        ("A * B *\nC", concat!("  A * B\n", "* C\n")),
        ("A -> B *\nC", concat!("   A\n", "->   B\n", "   * C\n")),
        ("A *\nB ->\nC", concat!("     A\n", "   * B\n", "-> C\n")),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn leads_infix_operators_when_the_inline_chain_exceeds_the_line_width() {
    let options = PrettyOptions::default()
        .with_line_width(12)
        .with_layout_intentions(LayoutIntentions::Ignore);
    let cases = [
        ("Alpha * Beta * Gamma", concat!("  Alpha\n", "* Beta\n", "* Gamma\n")),
        ("Alpha -> Beta -> Gamma", concat!("   Alpha\n", "-> Beta\n", "-> Gamma\n")),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(options);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(options));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn starts_a_hanging_infix_chain_at_its_enclosing_layout_boundary() {
    let cases = [
        (
            "(#field :: A *\nB *\nC)",
            concat!("(\n", "  #field ::\n", "    A\n", "  * B\n", "  * C\n", ")\n"),
        ),
        (
            "data\n| +Pair : A *\nB\nend",
            concat!("data\n", "| +Pair :\n", "    A\n", "  * B\n", "end\n"),
        ),
        (
            "codata\n| .call : A ->\nB\nend",
            concat!("codata\n", "| .call :\n", "     A\n", "  -> B\n", "end\n"),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(formatted, expected, "source: {source}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn preserves_comments_around_hanging_infix_chains() {
    let cases = [
        (
            concat!("A *\n", "-- Keep this operand note.\n", "B"),
            concat!("  A *\n", "  -- Keep this operand note.\n", "  B\n"),
        ),
        (
            concat!("A *\n", "-- Keep this operand note.\n", "\n", "B"),
            concat!("  A *\n", "  -- Keep this operand note.\n", "\n", "  B\n"),
        ),
        (
            concat!("A *\n", "\n", "-- Keep this operand note.\n", "B"),
            concat!("  A *\n", "\n", "  -- Keep this operand note.\n", "  B\n"),
        ),
        (
            "/- Keep this prefix. -/ A *\nB",
            "/- Keep this prefix. -/ A\n                      * B\n",
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(formatted, expected, "source: {source}");
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn keeps_placement_with_short_context_bindees() {
    let cases = [
        ("let value = item in value", "let value = item in\nvalue\n"),
        ("def value = item that value", "def value = item that\nvalue\n"),
        (
            "let Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nthat\nvalue",
            "let Cmp (A : VType) = Thk (A -> A -> Ret Bool) that\nvalue\n",
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, expected, "source: {source}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn placement_tiers_mirror_the_definition_stage() {
    let cases = [
        // A single-line bindee keeps the marker on its own line.
        (
            concat!("begin\n", "  def value = item that\n", "  value\n", "end\n",),
            concat!("begin\n", "  def value = item that\n", "  value\n", "end\n",),
        ),
        // A multiline delimited bindee whose closer returns to the
        // binding level takes the attached tier.
        (
            concat!("begin\n", "  let value = begin\n", "  item\n", "  end in value\n", "end\n",),
            concat!(
                "begin\n",
                "  let value = begin\n",
                "    item\n",
                "  end in\n",
                "  value\n",
                "end\n",
            ),
        ),
        // A broken bindee hangs one level below the binding, so the
        // marker breaks onto its own line at the binding indentation.
        (
            concat!("begin\n", "  let value =\n", "    item\n", "  in value\n", "end\n",),
            concat!("begin\n", "  let value =\n", "    item\n", "  in\n", "  value\n", "end\n",),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn placement_closes_at_the_binding_indentation() {
    let cases = [
        // A joined multiline bindee sits at the binding level; the
        // placement must not outdent below it.
        (
            concat!("begin\n", "  let value = begin\n", "  item\n", "  end in value\n", "end\n",),
            concat!(
                "begin\n",
                "  let value = begin\n",
                "    item\n",
                "  end in\n",
                "  value\n",
                "end\n",
            ),
        ),
        // A broken bindee nests one level below the binding; the
        // placement returns to the binding level.
        (
            concat!("begin\n", "  let value =\n", "    item\n", "  in value\n", "end\n",),
            concat!("begin\n", "  let value =\n", "    item\n", "  in\n", "  value\n", "end\n",),
        ),
        // A multiline type breaks the definition stage, and its outdent
        // already returns the bindee to the binding level.
        (
            concat!(
                "begin\n",
                "  def branch : Thk (\n",
                "    forall (B : CType) .\n",
                "      Bool -> Thk B -> Thk B -> B\n",
                "  ) = {\n",
                "    fn (B : CType)\n",
                "       condition\n",
                "       when_true\n",
                "       when_false =>\n",
                "      match condition\n",
                "      | +True() => ! when_true\n",
                "      | +False() => ! when_false\n",
                "      end\n",
                "  } that\n",
                "  branch\n",
                "end\n",
            ),
            concat!(
                "begin\n",
                "  def branch : Thk (\n",
                "    forall (B : CType) .\n",
                "      Bool -> Thk B -> Thk B -> B\n",
                "  ) = {\n",
                "    fn (B : CType)\n",
                "      condition\n",
                "      when_true\n",
                "      when_false\n",
                "    =>\n",
                "      match condition\n",
                "      | +True() => ! when_true\n",
                "      | +False() => ! when_false\n",
                "      end\n",
                "  } that\n",
                "  branch\n",
                "end\n",
            ),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn breaks_placement_after_multiline_bindings() {
    let cases = [
        (
            "let value = A *\nB in value",
            concat!("let value =\n", "    A\n", "  * B\n", "in\n", "value\n"),
        ),
        (
            "def value = A ->\nB that value",
            concat!("def value =\n", "     A\n", "  -> B\n", "that\n", "value\n"),
        ),
        (
            "let value = begin\nitem\nend in value",
            concat!("let value = begin\n", "  item\n", "end in\n", "value\n"),
        ),
        (
            "let Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nthat\nvalue",
            concat!("let Cmp (A : VType) =\n", "  Thk (A -> A -> Ret Bool)\n", "that\n", "value\n",),
        ),
        (
            "let Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nin\nvalue",
            concat!("let Cmp (A : VType) =\n", "  Thk (A -> A -> Ret Bool)\n", "in\n", "value\n",),
        ),
        (
            "def Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nthat\nvalue",
            concat!("def Cmp (A : VType) =\n", "  Thk (A -> A -> Ret Bool)\n", "that\n", "value\n",),
        ),
        (
            "def Cmp (A : VType) =\n  Thk (A -> A -> Ret Bool)\nin\nvalue",
            concat!("def Cmp (A : VType) =\n", "  Thk (A -> A -> Ret Bool)\n", "in\n", "value\n",),
        ),
        (
            concat!(
                "begin\n",
                "  let Cmp (A : VType) =\n",
                "    Thk (A -> A -> Ret Bool)\n",
                "  that\n",
                "  value\n",
                "end",
            ),
            concat!(
                "begin\n",
                "  let Cmp (A : VType) =\n",
                "    Thk (A -> A -> Ret Bool)\n",
                "  that\n",
                "  value\n",
                "end\n",
            ),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn definition_separator_tiers_share_the_binding_indentation() {
    let cases = [
        // A delimited type whose closer returns to the binding level
        // takes the attached tier: `)` and `=` share the closer's line.
        (
            concat!(
                "begin\n",
                "  def f : (\n",
                "    Result\n",
                "  ) = value that\n",
                "  f\n",
                "end\n",
            ),
            concat!(
                "begin\n",
                "  def f : (\n",
                "    Result\n",
                "  ) = value that\n",
                "  f\n",
                "end\n",
            ),
        ),
        // A blank line between the type and the separator excludes the
        // attached tier, so the blank survives in the aligned form.
        (
            concat!(
                "begin\n",
                "  def f : (\n",
                "    Result\n",
                "  )\n",
                "\n",
                "  = value that\n",
                "  f\n",
                "end\n",
            ),
            concat!(
                "begin\n",
                "  def f : (\n",
                "    Result\n",
                "  )\n",
                "  =\n",
                "\n",
                "    value\n",
                "  that\n",
                "  f\n",
                "end\n",
            ),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert!(formatted.lines().all(|line| line.trim_end() == line));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn preserves_an_aligned_definition_separator_after_a_parameter_telescope() {
    let source = concat!(
        "begin\n",
        "  let Algebra\n",
        "    (M : VType -> CType) (R : CType)\n",
        "  =\n",
        "    forall (A : VType) .\n",
        "         Thk (M A)\n",
        "      -> Thk (A -> R)\n",
        "      -> R\n",
        "  in\n",
        "  Algebra\n",
        "end\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);

    assert_eq!(formatted, source);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn aligns_binding_separators_after_multiline_parameters_and_types() {
    let cases = [
        (
            concat!(
                "begin\n",
                "  let f (first : Type)\n",
                "    (second : Type) : Result = value in\n",
                "  f\n",
                "end",
            ),
            concat!(
                "begin\n",
                "  let f (first : Type)\n",
                "    (second : Type)\n",
                "  : Result = value in\n",
                "  f\n",
                "end\n",
            ),
        ),
        (
            concat!(
                "begin\n",
                "  def f : (\n",
                "    Result\n",
                "  ) = value that\n",
                "  f\n",
                "end",
            ),
            concat!(
                "begin\n",
                "  def f : (\n",
                "    Result\n",
                "  ) = value that\n",
                "  f\n",
                "end\n",
            ),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn aligns_binding_separators_after_width_wrapping() {
    let options = PrettyOptions::default()
        .with_line_width(38)
        .with_layout_intentions(LayoutIntentions::Ignore);
    let cases = [
        (
            "let f (first : FirstClassifier) (second : SecondClassifier) = value in f",
            concat!(
                "let f\n",
                "  (first : FirstClassifier)\n",
                "  (second : SecondClassifier)\n",
                "=\n",
                "  value\n",
                "in\n",
                "f\n",
            ),
        ),
        (
            "let f (first : FirstClassifier) (second : SecondClassifier) : Result = value in f",
            concat!(
                "let f\n",
                "  (first : FirstClassifier)\n",
                "  (second : SecondClassifier)\n",
                ": Result = value in\n",
                "f\n",
            ),
        ),
        (
            "def f : FirstClassifier * SecondClassifier * ThirdClassifier = value that f",
            concat!(
                "def f :\n",
                "    FirstClassifier * SecondClassifier\n",
                "  * ThirdClassifier\n",
                "= value that\n",
                "f\n",
            ),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(options);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(options));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn breaks_placement_after_width_wrapped_context_bindees() {
    let source = "let value = Alpha * Beta * Gamma in value";
    let expected = concat!("let value =\n", "    Alpha * Beta\n", "  * Gamma\n", "in\n", "value\n");
    let options = PrettyOptions::default()
        .with_line_width(20)
        .with_layout_intentions(LayoutIntentions::Ignore);
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render_with_options(options);

    assert_eq!(formatted, expected);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render_with_options(options));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn outdents_placement_after_long_context_bindees() {
    let options = PrettyOptions::default()
        .with_line_width(20)
        .with_layout_intentions(LayoutIntentions::Ignore);
    let cases = [
        (
            "let value = extraordinarily_long_bindee in value",
            concat!("let value =\n", "  extraordinarily_long_bindee\n", "in\n", "value\n"),
        ),
        (
            "def value = extraordinarily_long_bindee that value",
            concat!("def value =\n", "  extraordinarily_long_bindee\n", "that\n", "value\n",),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(options);
        assert_eq!(formatted, expected, "source: {source}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn compacts_manifest_existentials_when_grouping_is_semantically_transparent() {
    let parsed = ParsedSource::new("exists (#Counter = ((Counter as Int) : VType)) . Counter");

    assert_eq!(
        parsed.render(LayoutIntentions::Ignore),
        "exists (= Counter as Int : VType) . Counter\n"
    );
    let reparsed = ParsedSource::new(&parsed.render(LayoutIntentions::Ignore));
    assert_eq!(
        reparsed.render(LayoutIntentions::Ignore),
        "exists (= Counter as Int : VType) . Counter\n"
    );
}

#[test]
fn preserves_comments_on_manifest_parameter_wrappers() {
    let source = concat!(
        "exists (#outer =\n",
        "  -- Keep the inner field.\n",
        "  #inner = ((value as Definition) : Classifier)) . body",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);
    let reparsed = ParsedSource::new(&formatted);

    assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
}

#[test]
fn preserves_observed_line_boundaries() {
    let parsed = ParsedSource::new("(#first = first,\n#second = second)");

    assert_eq!(parsed.render(LayoutIntentions::Preserve), "(= first,\n  = second)\n");
    assert_eq!(parsed.render(LayoutIntentions::Ignore), "(= first, = second)\n");
}

#[test]
fn preserves_the_boundary_before_an_existential_parameter() {
    let source = "exists\n  (value : Type) . value";
    let expected = concat!("exists\n", "  (value : Type)\n", ".\n", "  value\n");
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);

    assert_eq!(formatted, expected);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
}

#[test]
fn preserves_multiple_items_grouped_on_each_source_line() {
    let source =
        concat!("(#first = first, #second = second,\n", "#third = third, #fourth = fourth)",);
    let parsed = ParsedSource::new(source);

    assert_eq!(
        parsed.render(LayoutIntentions::Preserve),
        concat!("(= first, = second,\n", "  = third, = fourth)\n")
    );
}

#[test]
fn expands_the_immediate_layer_of_an_overflowing_delimited_group() {
    let joined = concat!(
        "match left\n",
        "| +Node(/left = left_left; /value = left_value; ",
        "/tree_height = left_height; /right = left_right) => ret +Leaf()\n",
        "end",
    );
    let source_grouped = concat!(
        "match left\n",
        "| +Node(\n",
        "  /left = left_left; /value = left_value; ",
        "/tree_height = left_height; /right = left_right\n",
        ") => ret +Leaf()\n",
        "end",
    );
    let expected = concat!(
        "match left\n",
        "| +Node(\n",
        "  /left = left_left;\n",
        "  /value = left_value;\n",
        "  /tree_height = left_height;\n",
        "  /right = left_right\n",
        ") => ret +Leaf()\n",
        "end\n",
    );
    let options = PrettyOptions::default().with_line_width(80);

    [joined, source_grouped].into_iter().for_each(|source| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(options);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(formatted, reparsed.render_with_options(options));
    });
}

#[test]
fn canonicalizes_multiline_parameter_annotations() {
    let source = concat!(
        "begin\n",
        "  param (\n",
        "    (/VType; /CType; /Thk; /Ret; /Unit; /Int; /String; /OS; /int; /string; /stdio; /process) :\n",
        "    @(import(\"package.zy\"))\n",
        "  ) that\n",
        "  _\n",
        "end\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render_with_options(PrettyOptions::default());

    assert_eq!(
        formatted,
        concat!(
            "begin\n",
            "  param (/VType; /CType; /Thk; /Ret; /Unit; /Int; /String; /OS; /int; /string; /stdio; /process) :\n",
            "  @(import(\"package.zy\")) that\n",
            "  _\n",
            "end\n",
        )
    );
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());

    let narrow = parsed.render_with_options(PrettyOptions::default().with_line_width(90));
    assert!(narrow.lines().all(|line| line.len() <= 90));
    assert!(narrow.contains("/stdio;\n    /process\n  ) : @(import"), "{narrow}");
    let reparsed = ParsedSource::new(&narrow);
    assert_eq!(narrow, reparsed.render_with_options(PrettyOptions::default().with_line_width(90)));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn accounts_for_a_staged_marker_in_the_line_width() {
    let source = concat!(
        "begin\n",
        "  param (\n",
        "    (/VType; /CType; /Thk; /Ret; /Unit; /Int; /Float64; /Char; /String; /Addr; /Access; /Reader; /Writer; /OS) :\n",
        "    @(import(\"package.zy\"))\n",
        "  ) in\n",
        "  _\n",
        "end\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render_with_options(PrettyOptions::default());

    assert!(formatted.lines().all(|line| line.len() <= 100), "{formatted}");
    assert!(formatted.contains("/OS\n  ) : @(import"), "{formatted}");
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
}

#[test]
fn merges_joined_consecutive_scopes_into_one_telescope() {
    let cases = [
        // A group of parameters and a nested function are different programs.
        ("fn a => fn b => body", "fn a => fn b => body\n"),
        ("fn a b => fn c d => body", "fn a b => fn c d => body\n"),
        ("pi (A : VType) . pi (B : VType) . Body", "pi (A : VType) . pi (B : VType) . Body\n"),
        (
            "forall (A : VType) . forall (B : VType) . Body",
            "forall (A : VType) . forall (B : VType) . Body\n",
        ),
        ("sigma (A : VType) . sigma (B : VType) . Body", "sigma (A : VType) (B : VType) . Body\n"),
        (
            "exists (A : VType) . exists (B : VType) . Body",
            "exists (A : VType) (B : VType) . Body\n",
        ),
        (
            concat!("exists (A : VType) (B : VType) . ", "exists (C : VType) (D : VType) . Body",),
            "exists (A : VType) (B : VType) (C : VType) (D : VType) . Body\n",
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    });
}

#[test]
fn preserves_a_nested_scope_when_its_introducer_starts_on_a_new_line() {
    let cases = [
        (
            concat!("fn a =>\n", "  fn b =>\n", "    body"),
            concat!("fn a =>\n", "  fn b =>\n", "    body\n"),
        ),
        (
            concat!("forall (A : VType) .\n", "  forall (B : VType) .\n", "    Body"),
            concat!("forall (A : VType) .\n", "  forall (B : VType) .\n", "    Body\n"),
        ),
        (
            concat!("exists (A : VType) .\n", "  exists (B : VType) .\n", "    Body"),
            concat!("exists (A : VType) .\n", "  exists (B : VType) .\n", "    Body\n"),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    });
}

#[test]
fn keeps_a_parenthesized_application_head() {
    let cases = [("(f x) y", "(f x) y\n"), ("f x y", "f x y\n"), ("(f) x", "f x\n")];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, expected, "source: {source}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn keeps_comments_and_distinct_scope_forms_at_telescope_boundaries() {
    let cases = [
        (
            "fn a => /- Keep the nested scope. -/ fn b => body",
            "fn a => /- Keep the nested scope. -/ fn b => body\n",
        ),
        (
            "pi (A : VType) . forall (B : VType) . Body",
            "pi (A : VType) . forall (B : VType) . Body\n",
        ),
        (
            concat!(
                "exists (A : VType) . /- Keep the nested scope. -/ ",
                "exists (B : VType) . Body",
            ),
            concat!(
                "exists (A : VType) . /- Keep the nested scope. -/ ",
                "exists (B : VType) . Body\n",
            ),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    });
}

#[test]
fn multiline_groups_keep_their_delimiters() {
    let cases = [
        // A group whose contents break keeps its delimiters hugging the
        // enclosing line, with the contents nested inside.
        ("let x = (a *\nb) in x", concat!("let x = (\n", "    a\n", "  * b\n", ") in\n", "x\n")),
        // A parameter keyword already owns its binder extent, so source
        // breaks do not retain a redundant singleton wrapper.
        ("param (x :\nLongTypeName) that\nx", "param x : LongTypeName that\nx\n"),
        ("let x = (\n  a\n) in x", concat!("let x = (\n", "  a\n", ") in\n", "x\n")),
        // A single-line group still elides, and an application's wrapper
        // remains redundant even for multiline arguments.
        ("let x = (a) in x", "let x = a in\nx\n"),
        ("! f ((a))", "! f a\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn arm_blocks_start_on_their_own_lines() {
    let cases = [
        // An arm block anchored mid-line moves to its own line, and its
        // arms align with the block keyword.
        (
            concat!(
                "def Option (A : VType) = data\n",
                "| +None : Unit\n",
                "| +Some : A\n",
                "end that\n",
                "option",
            ),
            concat!(
                "def Option (A : VType) =\n",
                "  data\n",
                "  | +None : Unit\n",
                "  | +Some : A\n",
                "  end\n",
                "that\n",
                "option\n",
            ),
        ),
        (
            concat!(
                "def ! height (tree : Avl A) : Ret Int = match tree\n",
                "| +Leaf() => ret 0\n",
                "| +Node(/tree_height) => ret tree_height\n",
                "end that\n",
                "height",
            ),
            concat!(
                "def ! height (tree : Avl A) : Ret Int =\n",
                "  match tree\n",
                "  | +Leaf() => ret 0\n",
                "  | +Node(/tree_height) => ret tree_height\n",
                "  end\n",
                "that\n",
                "height\n",
            ),
        ),
        (
            "fn x => match x\n| +A() => ret 0\n| +B() => ret 1\nend",
            concat!(
                "fn x =>\n",
                "  match x\n",
                "  | +A() => ret 0\n",
                "  | +B() => ret 1\n",
                "  end\n",
            ),
        ),
        // An arm block already at a line start stays there.
        ("match x\n| +A() => ret 0\nend", "match x\n| +A() => ret 0\nend\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn sequence_bindings_start_on_their_own_lines() {
    let cases = [
        // A sequence binding anchored mid-line moves to its own line
        // because its tail marker always breaks.
        (
            "let x = do a <- b; ret a in x",
            concat!("let x =\n", "  do a <- b;\n", "  ret a\n", "in\n", "x\n"),
        ),
        (
            "do x <- do y <- b; ret y;\nret x",
            concat!("do x <-\n", "  do y <- b;\n", "  ret y;\n", "ret x\n"),
        ),
        ("fn x => do y <- b; ret y", concat!("fn x =>\n", "  do y <- b;\n", "  ret y\n")),
        // A sequence binding already at a line start stays there.
        ("begin\n  do x <- f;\n  ret x\nend", "begin\n  do x <- f;\n  ret x\nend\n"),
        // A sequence binding nested in a delimited region starts on a
        // new line without moving the surrounding delimiters.
        (
            "let x = { let y = c in y } in x",
            concat!("let x = {\n", "  let y = c in\n", "  y\n", "} in\n", "x\n"),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn scope_separator_breaks_only_after_a_multiline_head() {
    let cases = [
        // A single-line head keeps its marker on the same line.
        ("pi (A : VType) . Result", "pi (A : VType) . Result\n"),
        // A multiline head ends with the marker alone on its own line.
        (
            concat!("pi\n", "  (A : VType)\n", "  (B : CType -> CType)\n", ". Result",),
            concat!("pi\n", "  (A : VType)\n", "  (B : CType -> CType)\n", ".\n", "  Result\n",),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn places_scope_separators_after_parameters_that_wrap() {
    let options = PrettyOptions::default()
        .with_line_width(38)
        .with_layout_intentions(LayoutIntentions::Ignore);
    let cases = [
        (
            "fn (first : FirstClassifier) (second : SecondClassifier) => result",
            concat!(
                "fn\n",
                "  (first : FirstClassifier)\n",
                "  (second : SecondClassifier)\n",
                "=>\n",
                "  result\n",
            ),
        ),
        (
            "pi (first : FirstClassifier) (second : SecondClassifier) . Result",
            concat!(
                "pi\n",
                "  (first : FirstClassifier)\n",
                "  (second : SecondClassifier)\n",
                ".\n",
                "  Result\n",
            ),
        ),
        (
            "forall (first : FirstClassifier) (second : SecondClassifier) . Result",
            concat!(
                "forall\n",
                "  (first : FirstClassifier)\n",
                "  (second : SecondClassifier)\n",
                ".\n",
                "  Result\n",
            ),
        ),
        (
            "sigma (first : FirstClassifier) (second : SecondClassifier) . Result",
            concat!(
                "sigma\n",
                "  (first : FirstClassifier)\n",
                "  (second : SecondClassifier)\n",
                ".\n",
                "  Result\n",
            ),
        ),
        (
            "exists (first : FirstClassifier) (second : SecondClassifier) . Result",
            concat!(
                "exists\n",
                "  (first : FirstClassifier)\n",
                "  (second : SecondClassifier)\n",
                ".\n",
                "  Result\n",
            ),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(options);
        assert_eq!(formatted, expected, "source: {source}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(options));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn pack_parameters_nest_once_at_preserved_boundaries() {
    let cases = [
        (
            concat!(
                "pack\n",
                "    -- Re-export the constructors.\n",
                "    (= Monad as Monad)\n",
                "\n",
                "    (= Algebra as Algebra)\n",
                "    (= List : VType -> VType) is List\n",
                "-- Package operations.\n",
                "where\n",
                "  ()\n",
                "end\n",
            ),
            concat!(
                "pack\n",
                "  -- Re-export the constructors.\n",
                "  (= Monad as Monad)\n",
                "\n",
                "  (= Algebra as Algebra)\n",
                "  (= List : VType -> VType) is List\n",
                "-- Package operations.\n",
                "where\n",
                "  ()\n",
                "end\n",
            ),
        ),
        (
            concat!(
                "begin\n",
                "  pack\n",
                "      (= Monad as Monad)\n",
                "      (= Algebra as Algebra)\n",
                "  where\n",
                "    ()\n",
                "  end\n",
                "end\n",
            ),
            concat!(
                "begin\n",
                "  pack\n",
                "    (= Monad as Monad)\n",
                "    (= Algebra as Algebra)\n",
                "  where\n",
                "    ()\n",
                "  end\n",
                "end\n",
            ),
        ),
        (
            "pack (= Monad as Monad)\n    (= Algebra as Algebra)\nwhere () end\n",
            "pack (= Monad as Monad)\n  (= Algebra as Algebra)\nwhere () end\n",
        ),
    ];
    for (source, expected) in cases {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }
}

#[test]
fn pack_parameters_nest_once_when_expanding_for_width() {
    for indent in [1, 2, 4] {
        for layout in
            [LayoutIntentions::Preserve, LayoutIntentions::BlankLinesOnly, LayoutIntentions::Ignore]
        {
            let options = PrettyOptions::default()
                .with_indent(IndentWidth::new(indent).unwrap())
                .with_line_width(22)
                .with_layout_intentions(layout);
            let step = " ".repeat(indent);
            for (source, expected) in [
                (
                    "pack (A as Int) (B as Char) where 1, 'a' end",
                    format!("pack\n{step}(A as Int)\n{step}(B as Char)\nwhere 1, 'a' end\n"),
                ),
                (
                    "begin pack (A as Int) (B as Char) where 1, 'a' end end",
                    format!(
                        "begin\n{step}pack\n{step}{step}(A as Int)\n\
                             {step}{step}(B as Char)\n{step}where 1, 'a' end\nend\n"
                    ),
                ),
            ] {
                let parsed = ParsedSource::new(source);
                let formatted = parsed.render_with_options(options);
                assert_eq!(formatted, expected, "indent: {indent}, layout: {layout:?}");
                let reparsed = ParsedSource::new(&formatted);
                assert_eq!(formatted, reparsed.render_with_options(options));
                assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
            }
        }
    }
}

#[test]
fn preserves_fitting_parameter_rows_inside_multiline_telescopes() {
    let cases = [
        (
            concat!(
                "def ! unwrap_or (A : VType)\n",
                "  (E : VType)\n",
                "  (result : Result A E)\n",
                "  (default : A) : Ret A = value that\n",
                "unwrap_or",
            ),
            concat!(
                "def ! unwrap_or (A : VType)\n",
                "  (E : VType)\n",
                "  (result : Result A E)\n",
                "  (default : A)\n",
                ": Ret A = value that\n",
                "unwrap_or\n",
            ),
        ),
        (
            concat!(
                "let f (A : VType) (B : VType)\n",
                "  (left : A) (right : B) : Result = value in\n",
                "f",
            ),
            concat!(
                "let f (A : VType) (B : VType)\n",
                "  (left : A) (right : B)\n",
                ": Result = value in\n",
                "f\n",
            ),
        ),
        (
            concat!("forall (A : VType) (B : VType)\n", "  (C : VType) (D : VType) . Body",),
            concat!(
                "forall (A : VType) (B : VType)\n",
                "  (C : VType) (D : VType)\n",
                ".\n",
                "  Body\n",
            ),
        ),
        (
            concat!("exists (A : VType) (B : VType)\n", "  (C : VType) (D : VType) . Body",),
            concat!(
                "exists (A : VType) (B : VType)\n",
                "  (C : VType) (D : VType)\n",
                ".\n",
                "  Body\n",
            ),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn preserves_comments_inside_multiline_parameter_telescopes() {
    let source = concat!(
        "fn\n",
        "  -- Keep the first parameter.\n",
        "  (A : VType)\n",
        "  -- Keep the second parameter.\n",
        "  (B : VType) => Body",
    );
    let expected = concat!(
        "fn\n",
        "  -- Keep the first parameter.\n",
        "  (A : VType)\n",
        "  -- Keep the second parameter.\n",
        "  (B : VType)\n",
        "=>\n",
        "  Body\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);

    assert_eq!(formatted, expected);
    assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn places_function_separator_after_a_preserved_multiline_pattern() {
    let source = concat!("fix (\n", "  value\n", ") => result");
    let expected = concat!("fix (\n", "  value\n", ")\n", "=>\n", "  result\n");
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);

    assert_eq!(formatted, expected);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn does_not_pad_comment_driven_line_breaks() {
    let source =
        concat!("exists\n", "  --| Parameter documentation.\n", "\n", "  (Value : Type) . Value",);
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);

    assert_eq!(
        formatted,
        concat!(
            "exists\n",
            "  --| Parameter documentation.\n",
            "\n",
            "  (Value : Type)\n",
            ".\n",
            "  Value\n",
        )
    );
    assert!(formatted.lines().all(|line| line.trim_end() == line));

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn preserves_a_single_line_break_after_nested_comments() {
    let source =
        concat!("exists\n", "  --| Parameter documentation.\n", "  (Value : Type) . Value",);
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);

    assert_eq!(
        formatted,
        concat!(
            "exists\n",
            "  --| Parameter documentation.\n",
            "  (Value : Type)\n",
            ".\n",
            "  Value\n",
        )
    );

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn keeps_commented_manifest_parameters_on_their_written_line() {
    [
        concat!(
            "exists\n",
            "  -- Compiler-canonical type constructors.\n",
            "  (= Thk as @(intrinsic(thk)) : CType -> VType)\n",
            "  (= Ret as @(intrinsic(ret)) : VType -> CType)\n",
            ".\n",
            "  _",
        ),
        concat!(
            "exists\n",
            "  -- A named manifest field.\n",
            "  (#num = n as @(intrinsic(thk)) : CType -> VType)\n",
            ".\n",
            "  _",
        ),
        concat!(
            "exists\n",
            "  -- A plain annotated binder.\n",
            "  (value : Type)\n",
            ".\n",
            "  value",
        ),
    ]
    .into_iter()
    .for_each(|source| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(formatted, format!("{source}\n"));
        assert_eq!(parsed.desugared_shape(), ParsedSource::new(&formatted).desugared_shape());
    });
}

#[test]
fn retains_a_line_break_after_a_manifest_parameter_open_delimiter() {
    [
        concat!(
            "exists\n",
            "  (\n",
            "    = Thk as @(intrinsic(thk)) : CType -> VType)\n",
            ".\n",
            "  _",
        ),
        concat!(
            "exists\n",
            "  -- A comment above the open delimiter.\n",
            "  (\n",
            "    = Thk as @(intrinsic(thk)) : CType -> VType)\n",
            ".\n",
            "  _",
        ),
    ]
    .into_iter()
    .for_each(|source| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);

        assert_eq!(formatted, format!("{source}\n"));
        assert_eq!(parsed.desugared_shape(), ParsedSource::new(&formatted).desugared_shape());
    });
}

#[test]
fn keeps_commented_telescope_rows_on_separate_lines() {
    let source = concat!(
        "exists\n",
        "  (VType as @(intrinsic(vtype)))\n",
        "  -- Kind of value types.\n",
        "\n",
        "  (= Unit as @(intrinsic(unit)) : VType)\n",
        ".\n",
        "  _",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);

    assert_eq!(
        formatted,
        concat!(
            "exists\n",
            "  (VType as @(intrinsic(vtype)))\n",
            "  -- Kind of value types.\n",
            "\n",
            "  (= Unit as @(intrinsic(unit)) : VType)\n",
            ".\n",
            "  _\n",
        )
    );
    assert_eq!(parsed.desugared_shape(), ParsedSource::new(&formatted).desugared_shape());
}

#[test]
fn preserves_text_blocks_around_formatted_syntax() {
    let source = concat!(
        "--| Package heading\n",
        "--|\n",
        "--| Package details.\n",
        "@[doc] begin\n",
        "  --| A documented binding.\n",
        "  let value = ((1)) that\n",
        "  --| Use the value.\n",
        "  value\n",
        "end\n",
        "--| Trailing note.\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Ignore);

    assert_eq!(
        formatted,
        concat!(
            "--| Package heading\n",
            "--|\n",
            "--| Package details.\n",
            "@[doc] begin\n",
            "  --| A documented binding.\n",
            "  let value = 1 that\n",
            "  --| Use the value.\n",
            "  value\n",
            "end\n",
            "--| Trailing note.\n",
        )
    );
    assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
}

#[test]
fn starts_documentation_on_the_anchored_syntax_line() {
    let source = "fn parameter => --| Body documentation.\n@[doc] body";
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Ignore);

    assert_eq!(
        formatted,
        concat!("fn parameter =>\n", "  --| Body documentation.\n", "  @[doc] body\n",)
    );
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn preserves_ordinary_and_nested_block_comments() {
    let source = concat!(
        "-- Package note.\n",
        "/- Outer note.\n",
        "   /- Nested note. -/\n",
        "-/\n",
        "begin\n",
        "  -- Binding note.\n",
        "  let value = ((1)) that\n",
        "  /- Use the\n",
        "     bound value. -/\n",
        "  value\n",
        "end\n",
        "-- End note.\n",
        "/- Final note. -/\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Ignore);

    assert_eq!(
        formatted,
        concat!(
            "-- Package note.\n",
            "/- Outer note.\n",
            "   /- Nested note. -/\n",
            "-/\n",
            "begin\n",
            "  -- Binding note.\n",
            "  let value = 1 that\n",
            "  /- Use the\n",
            "     bound value. -/\n",
            "  value\n",
            "end\n",
            "-- End note.\n",
            "/- Final note. -/\n",
        )
    );
    assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
}

#[test]
fn preserves_comment_ownership_around_arm_prefixes() {
    let cases = [
        concat!(
            "data\n",
            "| +First : Unit\n",
            "-- Before the second arm.\n",
            "| +Second :\n",
            "  -- The second payload.\n",
            "  Unit\n",
            "end",
        ),
        concat!(
            "match value\n",
            "| +Present(result) => ret result\n",
            "-- Before the fallback arm.\n",
            "| _ =>\n",
            "  -- The fallback body.\n",
            "  ret default\n",
            "end",
        ),
    ];

    cases.into_iter().for_each(|source| {
        let expected = format!("{source}\n");
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    });
}

#[test]
fn preserves_blank_lines_at_arm_block_boundaries() {
    let cases = [
        concat!(
            "match tree\n",
            "| +Leaf() => ret 0\n",
            "\n",
            "| +Node(/tree_height) => ret tree_height\n",
            "end\n",
        ),
        concat!("data\n", "\n", "| +A : Unit\n", "\n", "| +B : Unit\n", "\n", "end\n",),
        concat!(
            "comatch\n",
            "| .a => ret 0\n",
            "\n",
            "-- The second arm.\n",
            "| .b => ret 1\n",
            "end\n",
        ),
    ];

    cases.into_iter().for_each(|source| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, source, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn preserves_blank_lines_at_block_boundaries() {
    let cases = [
        concat!("begin\n", "\n", "  let x = f in\n", "  ret x\n", "end\n",),
        concat!("begin\n", "  let x = f in\n", "  ret x\n", "\n", "end\n",),
        concat!("begin\n", "\n", "  let x = f in\n", "  ret x\n", "\n", "end\n",),
    ];

    cases.into_iter().for_each(|source| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, source, "source: {source}");
        assert!(formatted.lines().all(|line| line.trim_end() == line));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn ignores_blank_lines_at_arm_boundaries_when_intentions_are_off() {
    let source = concat!(
        "match tree\n",
        "| +Leaf() => ret 0\n",
        "\n",
        "| +Node(/tree_height) => ret tree_height\n",
        "end\n",
    );
    let expected = concat!(
        "match tree\n",
        "| +Leaf() => ret 0\n",
        "| +Node(/tree_height) => ret tree_height\n",
        "end\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Ignore);

    assert_eq!(formatted, expected);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
}

#[test]
fn blank_lines_only_mode_rejoins_single_breaks_and_keeps_blank_lines() {
    let options = PrettyOptions::default().with_layout_intentions(LayoutIntentions::BlankLinesOnly);
    let cases = [
        // A hand-wrapped application rejoins when its compact form fits.
        (
            "! (bool/if)\n  (Ret Int)\n  greater\n  { ret left }\n  { ret right }",
            "! bool/if (Ret Int) greater { ret left } { ret right }\n",
        ),
        // A blank line still partitions the argument rows and survives.
        (
            "! (bool/if)\n  (Ret Int)\n\n  greater\n  { ret left }\n  { ret right }",
            "! bool/if (Ret Int)\n\n  greater { ret left } { ret right }\n",
        ),
        // A single infix break rejoins; a blank line keeps the hanging form.
        ("A *\nB", "A * B\n"),
        ("A *\n\nB", "  A\n\n* B\n"),
        // A single break before a nested scope no longer stops the fold.
        ("fn a =>\nfn b => body", "fn a => fn b => body\n"),
        // A blank line before the nested introducer still stops it,
        // while the inner scope remains compact when it fits.
        ("fn a =>\n\nfn b => body", "fn a =>\n\n  fn b => body\n"),
        // Blank lines between sequence stages survive.
        ("do x <- ! f a;\n\ndo y <- ! g b;\nret x", "do x <- ! f a;\n\ndo y <- ! g b;\nret x\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(options);
        assert_eq!(formatted, expected, "source: {source}");
        assert!(formatted.lines().all(|line| line.trim_end() == line));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(options), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn format_annotations_scope_layout_intentions() {
    let cases = [
        (
            concat!(
                "@[format(layout(ignore))] ! (bool/if)\n",
                "  (Ret Int)\n",
                "  greater\n",
                "  { ret left }\n",
                "  { ret right }\n",
            ),
            "@[format(layout(ignore))] ! bool/if (Ret Int) greater { ret left } { ret right }\n",
        ),
        (
            concat!(
                "@[format(layout(blank_lines))] ! (bool/if)\n",
                "  (Ret Int)\n",
                "\n",
                "  greater\n",
                "  { ret left }\n",
                "  { ret right }\n",
            ),
            concat!(
                "@[format(layout(blank_lines))] ! bool/if (Ret Int)\n",
                "\n",
                "  greater { ret left } { ret right }\n",
            ),
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert!(formatted.lines().all(|line| line.trim_end() == line));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn format_annotations_scope_indentation() {
    let source = concat!(
        "begin\n",
        "  @[format(indent(4))] begin\n",
        "  let x = f\n",
        "  in ret x\n",
        "  end\n",
        "end\n",
    );
    let expected = concat!(
        "begin\n",
        "  @[format(indent(4))] begin\n",
        "      let x = f in\n",
        "      ret x\n",
        "  end\n",
        "end\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);
    assert_eq!(formatted, expected, "source: {source}");

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn format_annotations_scope_line_width() {
    let cases = [
        (
            "@[format(width(24))] ! (bool/if) (Ret Int) greater { ret left } { ret right }\n",
            concat!(
                "@[format(width(24))]\n",
                "! bool/if (Ret Int)\n",
                "  greater { ret left } {\n",
                "  ret right\n",
                "}\n",
            ),
        ),
        (
            concat!(
                "@[format(width(20))] ! (bool/if)\n",
                "\n",
                "  (Ret Int)\n",
                "  greater\n",
                "  { ret left }\n",
                "  { ret right }\n",
            ),
            concat!(
                "@[format(width(20))]\n",
                "! bool/if\n",
                "\n",
                "  (Ret Int)\n",
                "  greater\n",
                "  { ret left }\n",
                "  { ret right }\n",
            ),
        ),
        ("@[format(width(24))] A * B\n", "@[format(width(24))] A * B\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert!(formatted.lines().all(|line| line.trim_end() == line), "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape(), "source: {source}");
    });
}

#[test]
fn format_annotations_compose_innermost_first() {
    let source = concat!(
        "@[format(width(24))] @[format(layout(ignore))] ! (bool/if)\n",
        "  (Ret Int)\n",
        "  greater\n",
        "  { ret left }\n",
        "  { ret right }\n",
    );
    let expected = concat!(
        "@[format(width(24))]\n",
        "@[format(layout(ignore))]\n",
        "! bool/if (Ret Int)\n",
        "  greater { ret left } {\n",
        "  ret right\n",
        "}\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);
    assert_eq!(formatted, expected, "source: {source}");

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn format_annotations_scope_whole_annotated_terms() {
    let source = concat!(
        "@[format(layout(ignore))] ! (bool/if)\n",
        "  (Ret Int)\n",
        "  greater\n",
        "  { ret left }\n",
        "  { ret right }\n",
        "* ! (other/if)\n",
        "  (Ret Int)\n",
        "  other\n",
        "  { ret a }\n",
        "  { ret b }\n",
    );
    let expected = concat!(
        "@[format(layout(ignore))] ! bool/if (Ret Int) greater { ret left } { ret right } * ! other/if (\n",
        "  Ret Int\n",
        ") other { ret a } { ret b }\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render_with_options(PrettyOptions::default());
    assert_eq!(formatted, expected, "source: {source}");
    assert!(formatted.lines().all(|line| line.len() <= 100), "{formatted}");

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn format_verbatim_keeps_long_payloads_on_one_line() {
    let source = concat!(
        "@[format(verbatim)] (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ",
        "16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, ",
        "35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, ",
        "54, 55, 56, 57, 58, 59, 60, 61, 62, 63)\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render_with_options(PrettyOptions::default().with_line_width(20));
    assert_eq!(formatted, source, "verbatim must not reflow the payload");

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn format_verbatim_preserves_multiline_source_and_comments() {
    let source = concat!(
        "begin\n",
        "  @[format(verbatim)]\n",
        "  -- Keep this comment.\n",
        "  (0,\n",
        "  1,\n",
        "  2)\n",
        "end\n",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render_with_options(PrettyOptions::default().with_line_width(20));
    assert_eq!(formatted, source, "verbatim must preserve the annotated source text");

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
}

#[test]
fn format_verbatim_accepts_bare_and_call_spellings() {
    let cases = [
        "@[format(verbatim)] (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)\n",
        "@[format(verbatim())] (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)\n",
    ];

    for source in cases {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(PrettyOptions::default().with_line_width(20));
        assert_eq!(formatted, source, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(PrettyOptions::default()));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }
}

#[test]
fn malformed_format_annotations_remain_inert() {
    let cases = [
        "@[format(nope(1))] (#field = field, ((x)))\n",
        "@[format(width(0))] (#field = field, ((x)))\n",
        "@[format(100)] (#field = field, ((x)))\n",
    ];

    cases.into_iter().for_each(|source| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(
            formatted,
            source.replace("(#field = field, ((x)))", "(= field, x)"),
            "source: {source}"
        );

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn blank_lines_only_mode_still_collapses_redundant_blank_runs() {
    let options = PrettyOptions::default().with_layout_intentions(LayoutIntentions::BlankLinesOnly);
    let cases =
        [("A *\n\n\n\nB", "  A\n\n* B\n"), ("(first,\n\n\nsecond)", "(first,\n\n  second)\n")];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(options);
        assert_eq!(formatted, expected, "source: {source}");
        assert!(formatted.lines().all(|line| line.trim_end() == line));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(options));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn blank_lines_only_mode_elides_parentheses_with_single_breaks_only() {
    let options = PrettyOptions::default().with_layout_intentions(LayoutIntentions::BlankLinesOnly);
    let cases = [
        // A single break inside a singleton group disappears with the group.
        ("(\nvalue\n)", "value\n"),
        // A blank line inside the group keeps the delimiters and the blank.
        ("(\n\nvalue\n)", "(\n\n  value)\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render_with_options(options);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render_with_options(options));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn preserves_line_separation_around_comments() {
    let cases = [
        ("/- Inline. -/value", "/- Inline. -/ value\n"),
        ("/- Above. -/\nvalue", "/- Above. -/\nvalue\n"),
        ("/- Detached. -/\n\nvalue", "/- Detached. -/\n\nvalue\n"),
        ("value/- Trailing. -/", "value /- Trailing. -/\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, expected, "source: {source}");
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
    });
}

#[test]
fn documentation_starts_a_fresh_line_after_a_same_line_comment() {
    let cases = [
        ("/- c -/ --| doc\nvalue", "/- c -/\n--| doc\nvalue\n"),
        ("/- a -/ /- b -/ --| doc\nvalue", "/- a -/ /- b -/\n--| doc\nvalue\n"),
        ("/- c -/ --| doc\n@[doc] value", "/- c -/\n--| doc\n@[doc] value\n"),
        // Other comments keep sharing a line, and a line comment already
        // ends its own.
        ("/- a -/ /- b -/ value", "/- a -/ /- b -/ value\n"),
        ("-- line\n--| doc\nvalue", "-- line\n--| doc\nvalue\n"),
        // Trailing documentation follows the same rule.
        ("value /- c -/ --| doc", "value /- c -/\n--| doc\n"),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve), "source: {source}");
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    });
}

#[test]
fn leading_comments_participate_in_layout_boundaries() {
    let source = concat!("begin value end\n", "-- Explain the argument.\n", "argument");
    let expected =
        concat!("begin\n", "  value\n", "end\n", "  -- Explain the argument.\n", "  argument\n",);
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);

    assert_eq!(formatted, expected);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
}

#[test]
fn prefix_forms_respect_leading_comment_boundaries() {
    let cases = [
        ("ret\n-- Explain the value.\nvalue", "ret\n  -- Explain the value.\n  value\n"),
        (
            "param\n-- Explain the binder.\nvalue that tail",
            "param\n-- Explain the binder.\nvalue that\ntail\n",
        ),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
    });
}

#[test]
fn block_constructs_start_their_own_line_after_leading_comments() {
    let cases = [
        // A same-line comment keeps its line, and the block construct
        // starts the next one.
        ("/- outer -/ let value = 'x' in value", "/- outer -/\nlet value = 'x' in\nvalue\n"),
        ("/- c -/ match x\n| +A() => ret 0\nend", "/- c -/\nmatch x\n| +A() => ret 0\nend\n"),
        // Only the separation before the construct breaks.
        (
            "/- first -/ /- second -/ let value = 'x' in value",
            "/- first -/ /- second -/\nlet value = 'x' in\nvalue\n",
        ),
        // A line comment already ends its line.
        ("-- line\nlet value = 'x' in value", "-- line\nlet value = 'x' in\nvalue\n"),
        // A term that may start mid-line stays beside its comment.
        ("/- c -/ value", "/- c -/ value\n"),
        // The comment starts the construct's layout, so a nested
        // construct moves to its own line together with its comments.
        (
            "let x = /- c -/ let y = 'a' in y in x",
            "let x =\n  /- c -/\n  let y = 'a' in\n  y\nin\nx\n",
        ),
        ("let x = -- c\nlet y = 'a' in y in x", "let x =\n  -- c\n  let y = 'a' in\n  y\nin\nx\n"),
    ];

    [LayoutIntentions::Preserve, LayoutIntentions::BlankLinesOnly, LayoutIntentions::Ignore]
        .into_iter()
        .for_each(|policy| {
            cases.into_iter().for_each(|(source, expected)| {
                let parsed = ParsedSource::new(source);
                let formatted = parsed.render(policy);
                assert_eq!(formatted, expected, "source: {source}");

                let reparsed = ParsedSource::new(&formatted);
                assert_eq!(formatted, reparsed.render(policy), "source: {source}");
                assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
                assert_eq!(
                    RetainedComments::collect(source),
                    RetainedComments::collect(&formatted)
                );
            });
        });
}

#[test]
fn boundaries_break_before_leading_comments_that_end_their_line() {
    // Each uncommented source keeps its joined boundary; its commented
    // counterpart breaks there, and the comment nests with its term.
    let pairs = [
        // An arm payload.
        [
            ("match x\n| +A() => ret 0\nend", "match x\n| +A() => ret 0\nend\n"),
            (
                "match x\n| +A() => -- note\nret 0\nend",
                "match x\n| +A() =>\n  -- note\n  ret 0\nend\n",
            ),
        ],
        // A block comment ends its line only before a source line break.
        [
            (
                "match x\n| +A() => /- note -/ ret 0\nend",
                "match x\n| +A() => /- note -/ ret 0\nend\n",
            ),
            (
                "match x\n| +A() => /- note -/\nret 0\nend",
                "match x\n| +A() =>\n  /- note -/\n  ret 0\nend\n",
            ),
        ],
        // Any comment that ends its line breaks the boundary, including
        // one before a same-line comment.
        [
            (
                "match x\n| +A() => /- a -/ /- b -/ ret 0\nend",
                "match x\n| +A() => /- a -/ /- b -/ ret 0\nend\n",
            ),
            (
                "match x\n| +A() => -- a\n/- b -/ ret 0\nend",
                "match x\n| +A() =>\n  -- a\n  /- b -/ ret 0\nend\n",
            ),
        ],
        // Documentation always ends its line.
        [
            ("match x\n| +A() => @[doc] ret 0\nend", "match x\n| +A() => @[doc] ret 0\nend\n"),
            (
                "match x\n| +A() => --| Explain.\n@[doc] ret 0\nend",
                "match x\n| +A() =>\n  --| Explain.\n  @[doc] ret 0\nend\n",
            ),
        ],
        // A binding's bindee.
        [
            ("let x = foo in x", "let x = foo in\nx\n"),
            ("let x = -- c\nfoo in x", "let x =\n  -- c\n  foo\nin\nx\n"),
        ],
        // An application argument.
        [("f a b", "f a b\n"), ("f a -- c\nb", "f a\n  -- c\n  b\n")],
        // An aligned boundary breaks without indentation.
        [("@[m] x", "@[m] x\n"), ("@[m] -- c\nx", "@[m]\n-- c\nx\n")],
        // An infix operand hangs the whole chain.
        [("A -> B", "A -> B\n"), ("A -> -- c\nB", "   A ->\n   -- c\n   B\n")],
        // A modifier leads the binding head, so a comment after it does
        // not start the continuation and the boundary before it stays
        // joined.
        [
            ("let fix x = 'a' in x", "let fix x = 'a' in\nx\n"),
            ("let fix -- c\nx = 'a' in x", "let fix -- c\nx\n=\n  'a'\nin\nx\n"),
        ],
    ];

    [LayoutIntentions::Preserve, LayoutIntentions::BlankLinesOnly, LayoutIntentions::Ignore]
        .into_iter()
        .for_each(|policy| {
            pairs.into_iter().flatten().for_each(|(source, expected)| {
                let parsed = ParsedSource::new(source);
                let formatted = parsed.render(policy);
                assert_eq!(formatted, expected, "source: {source}");

                let reparsed = ParsedSource::new(&formatted);
                assert_eq!(formatted, reparsed.render(policy), "source: {source}");
                assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
                assert_eq!(
                    RetainedComments::collect(source),
                    RetainedComments::collect(&formatted)
                );
            });
        });
}

#[test]
fn does_not_pun_away_payload_comments() {
    let cases = [
        "(#field =\n-- Keep the term payload.\nfield)",
        "param (#field =\n-- Keep the pattern payload.\nfield) that _",
    ];

    cases.into_iter().for_each(|source| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert!(formatted.contains("#field =\n"), "source: {source}\n{formatted}");

        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
        assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));
    });
}

#[test]
fn preserves_separation_of_detached_documentation() {
    [
        ("--| Detached\n\n@[doc] _", "--| Detached\n\n@(doc)\n"),
        ("--| Detached\n-- barrier\n@[doc] _", "--| Detached\n-- barrier\n@(doc)\n"),
        ("@[doc] value --| Detached", "@[doc] value\n--| Detached\n"),
    ]
    .into_iter()
    .for_each(|(source, expected)| {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Ignore);
        assert_eq!(formatted, expected);

        let reparsed = ParsedSource::new(&formatted);
        let documentation = reparsed
            .formatted
            .unit
            .documentation(&reparsed.formatted.arena, &reparsed.formatted.spans);
        let [site] = documentation.as_slice() else {
            panic!("expected one documentation annotation")
        };
        assert!(site.directive.comment.is_none());
    });
}

#[test]
fn parenthesized_metadata_defaults_its_payload_through_pretty_printing() {
    let source = "--| Attached text.\n@(doc)";
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Ignore);
    assert_eq!(formatted, "--| Attached text.\n@(doc)\n");

    let reparsed = ParsedSource::new(&formatted);
    let documentation =
        reparsed.formatted.unit.documentation(&reparsed.formatted.arena, &reparsed.formatted.spans);
    let [site] = documentation.as_slice() else { panic!("expected one documentation annotation") };
    assert_eq!(
        site.directive.comment.as_ref().map(|comment| comment.text.as_ref()),
        Some("Attached text."),
    );
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Ignore));
}

#[test]
fn metadata_with_a_commented_hole_payload_keeps_the_bracket_form() {
    let source = "@[doc]\n-- Keep the hole.\n_";
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Ignore);

    assert!(formatted.contains("@[doc]"), "{formatted}");
    assert!(formatted.contains("-- Keep the hole."), "{formatted}");
    assert!(!formatted.contains("@(doc)"), "{formatted}");

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn qualified_metadata_identifiers_round_trip_without_path_quotes_or_projection() {
    let source = "@[package(test(of(std/data)), name(std/data/smoke))] @(import(std/data))";
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Ignore);
    assert_eq!(formatted.trim(), source);
    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
}

#[test]
fn metadata_applications_expand_at_their_own_width_boundary() {
    let source = r#"@[example(name("zydeco"),version("0.3.0"),license("MIT"))] _"#;
    let parsed = ParsedSource::new(source);

    let wide = parsed.render_with_options(
        PrettyOptions::default()
            .with_line_width(120)
            .with_layout_intentions(LayoutIntentions::Ignore),
    );
    assert_eq!(wide, "@(example(name(\"zydeco\"), version(\"0.3.0\"), license(\"MIT\")))\n",);

    let narrow = parsed.render_with_options(
        PrettyOptions::default()
            .with_line_width(32)
            .with_layout_intentions(LayoutIntentions::Ignore),
    );
    assert_eq!(
        narrow,
        concat!(
            "@(example(\n",
            "  name(\"zydeco\"),\n",
            "  version(\"0.3.0\"),\n",
            "  license(\"MIT\")\n",
            "))\n",
        ),
    );

    let reparsed = ParsedSource::new(&narrow);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(
        narrow,
        reparsed.render_with_options(
            PrettyOptions::default()
                .with_line_width(32)
                .with_layout_intentions(LayoutIntentions::Ignore),
        ),
    );
}

#[test]
fn metadata_preserves_authored_rows_and_internal_comments() {
    let source = concat!(
        "@[example(name(\"zydeco\"),\n",
        "-- Explain the selected version.\n",
        "version(\"0.3.0\"))] _",
    );
    let parsed = ParsedSource::new(source);
    let formatted = parsed.render(LayoutIntentions::Preserve);

    assert_eq!(
        formatted,
        concat!(
            "@(example(\n",
            "  name(\"zydeco\"),\n",
            "  -- Explain the selected version.\n",
            "  version(\"0.3.0\")\n",
            "))\n",
        ),
    );
    assert_eq!(RetainedComments::collect(source), RetainedComments::collect(&formatted));

    let reparsed = ParsedSource::new(&formatted);
    assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
}

#[test]
fn metadata_comments_stay_on_their_side_of_the_annotation_prefix() {
    let before_source = concat!(
        "exists\n",
        "-- Describe the host reader.\n",
        "@[builtin(reader)] (Reader : VType) . Reader",
    );
    let before = ParsedSource::new(before_source);
    let before_formatted = before.render(LayoutIntentions::Preserve);
    let comment = before_formatted.find("-- Describe").unwrap();
    let annotation = before_formatted.find("@[builtin").unwrap();

    assert!(comment < annotation, "{before_formatted}");
    assert_eq!(
        RetainedComments::collect(before_source),
        RetainedComments::collect(&before_formatted),
    );
    let reparsed = ParsedSource::new(&before_formatted);
    assert_eq!(before.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(before_formatted, reparsed.render(LayoutIntentions::Preserve));

    let inside_source = concat!("@[\n", "-- Explain the metadata.\n", "package(name)] value");
    let inside = ParsedSource::new(inside_source);
    let inside_formatted = inside.render(LayoutIntentions::Preserve);
    assert_eq!(
        inside_formatted,
        concat!("@[\n", "  -- Explain the metadata.\n", "  package(name)\n", "]\n", "value\n",),
    );
    assert_eq!(
        RetainedComments::collect(inside_source),
        RetainedComments::collect(&inside_formatted),
    );
    let reparsed = ParsedSource::new(&inside_formatted);
    assert_eq!(inside.desugared_shape(), reparsed.desugared_shape());
    assert_eq!(inside_formatted, reparsed.render(LayoutIntentions::Preserve));
}

#[test]
fn seal_formatting_round_trips_and_parenthesizes_tighter_contexts() {
    for (source, expected) in [
        (
            "val (x : Int) => seal (= T, x) : exists (= T : VType) . T",
            "val (x : Int) => seal (= T, x) : exists (= T : VType) . T\n",
        ),
        (
            "let s =\n  seal\n    begin\n      (= T, 1)\n    end : exists (= T : VType) . T\nin\ns",
            concat!(
                "let s =\n",
                "  seal\n",
                "    begin\n",
                "      (= T, 1)\n",
                "    end : exists (= T : VType) . T\n",
                "in\n",
                "s\n",
            ),
        ),
        (
            "(seal (= T, 1) : exists (= T : VType) . T)/field",
            "(seal (= T, 1) : exists (= T : VType) . T)/field\n",
        ),
    ] {
        let parsed = ParsedSource::new(source);
        let formatted = parsed.render(LayoutIntentions::Preserve);
        assert_eq!(formatted, expected, "source: {source}");
        let reparsed = ParsedSource::new(&formatted);
        assert_eq!(formatted, reparsed.render(LayoutIntentions::Preserve));
        assert_eq!(parsed.desugared_shape(), reparsed.desugared_shape());
    }
}

#[test]
fn standard_library_pretty_printing_reparses_idempotently() {
    [
        ("builtin.zy", include_str!("../../../../lib/std/builtin.zy")),
        ("data-package.zy", include_str!("../../../../lib/std/data/package.zy")),
        ("monad.zy", include_str!("../../../../lib/std/effect/monad.zy")),
        ("numeric-package.zy", include_str!("../../../../lib/std/numeric/package.zy")),
        ("std.zy", include_str!("../../../../lib/std/std.zy")),
        ("system-package.zy", include_str!("../../../../lib/std/system/package.zy")),
        ("text-package.zy", include_str!("../../../../lib/std/text/package.zy")),
    ]
    .into_iter()
    .for_each(|(name, source)| {
        let original = ParsedSource::new(source);
        let first = original.render(LayoutIntentions::Ignore);
        let reparsed = ParsedSource::named(&first, name);
        let second = reparsed.render(LayoutIntentions::Ignore);
        assert_eq!(
            original.desugared_shape(),
            reparsed.desugared_shape(),
            "formatter changed the desugared structure of {name}"
        );
        assert_eq!(first, second, "formatter is not idempotent for {name}");
        assert_eq!(
            RetainedComments::collect(source),
            RetainedComments::collect(&first),
            "formatter changed comments in {name}"
        );
    });
}
