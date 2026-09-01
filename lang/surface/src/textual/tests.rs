use crate::{
    bitter::{SourceUnitDesugarer, fmt::Formatter as BitterFormatter, syntax as bitter},
    metadata::{BuiltinMetaError, IntrinsicMeta, IntrinsicMetaError, MonadicMetaError},
    textual::{
        arena::TextualScope,
        fmt::Formatter,
        syntax::{
            Alias, Ann, Appli, BindingFlavor, Block, BuiltinRole, BuiltinValueRole, CoPatId,
            ContextBind, DefId, DefinitionMode, Dtor, EntityId, ExistentialParameter, Exists, Hole,
            IntegerLiteral, IntegerOperation, IntegerType, IntrinsicRole, Label, Literal,
            ManifestPattern, Meta, MetaT, Named, Pack, Param, Paren, Parser, PatId, Pattern,
            Pipeline, PipelineDirection, Placement, Prod, Proj, ProjectionPattern, SourceUnit,
            Term, TermId, ViewPattern,
        },
    },
};
use zydeco_syntax::Ugly;
use zydeco_utils::{arena::IdAllocator, pass::CompilerPass};

use super::*;

#[test]
fn textual_entities_retain_their_category_tags() {
    let mut allocator = IdAllocator::<TextualScope>::new();
    let def: DefId = allocator.alloc();
    let pat: PatId = allocator.alloc();
    let copat: CoPatId = allocator.alloc();
    let term: TermId = allocator.alloc();

    assert!(matches!(EntityId::from(def), EntityId::Def(id) if id == def));
    assert!(matches!(EntityId::from(pat), EntityId::Pat(id) if id == pat));
    assert!(matches!(EntityId::from(copat), EntityId::CoPat(id) if id == copat));
    assert!(matches!(EntityId::from(term), EntityId::Term(id) if id == term));
}

#[test]
fn parsing_1() {
    let source = "!(!1)";
    let mut parser = Parser::new();
    let _ = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();
}
#[test]
fn parsing_2() {
    let source = "{ let x = 1 in ! exit x }";
    let mut parser = Parser::new();
    let _ = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();
}

#[test]
fn rejects_retired_do_tilde_syntax() {
    let retired = "do~ first; second";
    let mut parser = Parser::new();
    assert!(
        parser::SingleTermParser::new()
            .parse(retired, &mut parser, lexer::Lexer::new(retired))
            .is_err()
    );

    let explicit = "first { second }";
    let mut parser = Parser::new();
    parser::SingleTermParser::new()
        .parse(explicit, &mut parser, lexer::Lexer::new(explicit))
        .expect("explicit continuation application must remain available");
}

#[test]
fn rejects_retired_monadic_block_syntax() {
    let source = "monadic ret () end";
    let mut parser = Parser::new();
    assert!(
        parser::SingleTermParser::new()
            .parse(source, &mut parser, lexer::Lexer::new(source))
            .is_err(),
        "the delimited monadic keyword form must remain retired"
    );
}

#[test]
fn monadic_metadata_lowers_arbitrary_terms_to_monadic_blocks() {
    [("@[monadic] fn value => ret value", false), ("@[monadic] begin ret () end", true)]
        .into_iter()
        .for_each(|(source, expects_block_body)| {
            let mut parser = Parser::new();
            let unit = parser::SourceUnitParser::new()
                .parse(source, &mut parser, lexer::Lexer::new(source))
                .unwrap_or_else(|error| panic!("expected `{source}` to parse: {error}"));
            let output = SourceUnitDesugarer::new(&parser.spans, &parser.arena, unit)
                .run()
                .unwrap_or_else(|error| panic!("expected `{source}` to desugar: {error}"));
            let bitter::Term::MoBlock(block) = &output.arena.terms[&output.root] else {
                panic!("expected `{source}` to lower to a monadic block")
            };
            let body = &block.body;

            assert_eq!(
                matches!(output.arena.terms[body], bitter::Term::Block(_)),
                expects_block_body,
                "the annotation must preserve its payload term"
            );
        });
}

#[test]
fn monadic_metadata_rejects_arguments() {
    let source = "@[monadic(extra)] ret ()";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();
    let error = match SourceUnitDesugarer::new(&parser.spans, &parser.arena, unit).run() {
        | Ok(_) => panic!("monadic metadata must not accept arguments"),
        | Err(error) => error,
    };

    assert!(matches!(
        error,
        crate::bitter::DesugarError::InvalidMonadicMeta {
            source: MonadicMetaError::Arguments { found: 1 },
            ..
        }
    ));
}

#[test]
fn monadic_metadata_extent_survives_bitter_formatting() {
    let source = "(@[monadic] fn value => ret value) argument";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();
    let output = SourceUnitDesugarer::new(&parser.spans, &parser.arena, unit).run().unwrap();
    let rendered = output.root.ugly(&BitterFormatter::new(&output.arena));

    let mut reparsed = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(&rendered, &mut reparsed, lexer::Lexer::new(&rendered))
        .unwrap_or_else(|error| panic!("expected `{rendered}` to reparse: {error}"));
    let output = SourceUnitDesugarer::new(&reparsed.spans, &reparsed.arena, unit).run().unwrap();
    let bitter::Term::App(bitter::App(function, _)) = output.arena.terms[&output.root] else {
        panic!("expected the reparsed root to remain an application")
    };

    assert!(matches!(output.arena.terms[&function], bitter::Term::MoBlock(_)));
}

#[test]
fn retired_monadic_keywords_are_available_as_identifiers() {
    let source = "let monadic = 1 in let monadically = monadic in monadically";
    let mut parser = Parser::new();
    parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .expect("retired monadic keywords must lex as ordinary identifiers");
}

#[test]
fn parses_decimal_and_scientific_float_literals() {
    [("1.25", 1.25), ("-2.5e1", -25.0), ("1e3", 1000.0)].into_iter().for_each(
        |(source, expected)| {
            let mut parser = Parser::new();
            let term = parser::SingleTermParser::new()
                .parse(source, &mut parser, lexer::Lexer::new(source))
                .unwrap();
            let Term::Lit(Literal::Float(value)) = parser.arena.terms[&term] else {
                panic!("expected `{source}` to parse as a float literal")
            };
            assert_eq!(value.value(), expected);
        },
    );

    let source = "1";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();
    assert!(matches!(
        parser.arena.terms[&term],
        Term::Lit(Literal::Integer(IntegerLiteral::Unresolved(1)))
    ));
}

#[test]
fn separates_term_body_arrows_from_type_arrows() {
    [
        "A -> B",
        "fn value => ret value",
        "fix recur => ret ()",
        "match value | _ => ret () end",
        "comatch | .read => ret () end",
        "comatch value => ret value end",
    ]
    .into_iter()
    .for_each(|source| {
        let mut parser = Parser::new();
        parser::SingleTermParser::new()
            .parse(source, &mut parser, lexer::Lexer::new(source))
            .unwrap_or_else(|error| panic!("expected `{source}` to parse: {error}"));
    });

    [
        "A => B",
        "fn value -> ret value",
        "fix recur -> ret ()",
        "match value | _ -> ret () end",
        "comatch | .read -> ret () end",
        "comatch value -> ret value end",
    ]
    .into_iter()
    .for_each(|source| {
        let mut parser = Parser::new();
        assert!(
            parser::SingleTermParser::new()
                .parse(source, &mut parser, lexer::Lexer::new(source))
                .is_err(),
            "expected `{source}` to be rejected"
        );
    });
}

#[test]
fn metadata_preserves_identifiers_strings_integers_and_applications() {
    let source = r#"@[debug(name,"value",1,nested("path"))] _"#;
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Meta(MetaT(meta, payload)) = &parser.arena.terms[&term] else {
        panic!("expected a metadata term")
    };
    let Meta::Apply { callee, args } = meta else { panic!("expected applied metadata") };

    assert_eq!(callee, "debug");
    assert_eq!(
        args,
        &[
            Meta::ident("name"),
            Meta::string("value"),
            Meta::integer(1),
            Meta::apply("nested", [Meta::string("path")]),
        ]
    );
    assert!(matches!(parser.arena.terms[payload], Term::Hole(Hole)));
    assert_eq!(meta.to_string(), r#"debug(name,"value",1,nested("path"))"#);
}

#[test]
fn parenthesized_metadata_defaults_its_payload_to_a_hole() {
    let source = r#"@(debug(name,"value",1,nested("path")))"#;
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Meta(MetaT(meta, payload)) = &parser.arena.terms[&term] else {
        panic!("expected a metadata term")
    };
    assert_eq!(meta.to_string(), r#"debug(name,"value",1,nested("path"))"#);
    assert!(matches!(parser.arena.terms[payload], Term::Hole(Hole)));

    // The sugar renders in its parenthesized form and reparses.
    let rendered = term.ugly(&Formatter::new(&parser.arena));
    assert_eq!(rendered, source);
    let mut roundtrip = Parser::new();
    parser::SingleTermParser::new()
        .parse(&rendered, &mut roundtrip, lexer::Lexer::new(&rendered))
        .unwrap();

    // `@(meta)` and `@[meta] _` are indistinguishable once parsed.
    let bracketed = r#"@[debug(name,"value",1,nested("path"))] _"#;
    let mut bracketed_parser = Parser::new();
    let bracketed_term = parser::SingleTermParser::new()
        .parse(bracketed, &mut bracketed_parser, lexer::Lexer::new(bracketed))
        .unwrap();
    assert_eq!(
        term.ugly(&Formatter::new(&parser.arena)),
        bracketed_term.ugly(&Formatter::new(&bracketed_parser.arena)),
    );
}

#[test]
fn parenthesized_metadata_rejects_an_empty_annotation() {
    let source = "@()";
    let mut parser = Parser::new();
    assert!(
        parser::SingleTermParser::new()
            .parse(source, &mut parser, lexer::Lexer::new(source))
            .is_err(),
        "expected `{source}` to be rejected"
    );
}

#[test]
fn source_unit_collects_documentation_for_arbitrary_annotated_terms() {
    let source = concat!(
        "--| Package heading\n",
        "--|\n",
        "--| Package details.\n",
        "@[doc(section,\"api\",render(compact))] begin\n",
        "  let value =\n",
        "    --| An integer example.\n",
        "    @[doc(example)] 1\n",
        "  that\n",
        "  value\n",
        "end\n",
    );
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let documentation = unit.documentation(&parser.arena, &parser.spans);
    let [package, example] = documentation.as_slice() else {
        panic!("expected documentation on the root and a nested term")
    };

    assert_eq!(
        package.directive.meta.arguments,
        [
            Meta::ident("section"),
            Meta::string("api"),
            Meta::apply("render", [Meta::ident("compact")]),
        ]
    );
    assert_eq!(
        package.directive.comment.as_ref().unwrap().text.as_ref(),
        "Package heading\n\nPackage details."
    );
    assert!(matches!(parser.arena.terms[&package.payload], Term::Block(_)));

    assert_eq!(example.directive.meta.arguments, [Meta::ident("example")]);
    assert_eq!(example.directive.comment.as_ref().unwrap().text.as_ref(), "An integer example.");
    assert!(matches!(
        parser.arena.terms[&example.payload],
        Term::Lit(Literal::Integer(IntegerLiteral::Unresolved(1)))
    ));
}

#[test]
fn documentation_annotation_does_not_reach_across_a_blank_or_ordinary_comment() {
    ["--| Detached\n\n@[doc] _", "--| Detached\n-- barrier\n@[doc] _"].into_iter().for_each(
        |source| {
            let mut parser = Parser::new();
            let unit = parser::SourceUnitParser::new()
                .parse(source, &mut parser, lexer::Lexer::new(source))
                .unwrap();
            let documentation = unit.documentation(&parser.arena, &parser.spans);
            let [site] = documentation.as_slice() else {
                panic!("the annotation must include its term even without attached prose")
            };
            assert!(site.directive.meta.arguments.is_empty());
            assert!(site.directive.comment.is_none());
            assert!(matches!(parser.arena.terms[&site.payload], Term::Hole(Hole)));
        },
    );
}

#[test]
fn text_blocks_without_an_annotation_remain_unattached() {
    let source = "--| Informational only\n_";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    assert!(unit.documentation(&parser.arena, &parser.spans).is_empty());
}

#[test]
fn warns_for_every_text_block_without_an_effective_attachment() {
    let source = concat!(
        "(\n",
        "  --| Attached\n",
        "  @[doc] _,\n",
        "  --| Separated by a blank line\n",
        "\n",
        "  @[doc] _,\n",
        "  --| Interrupted by an ordinary comment\n",
        "  -- barrier\n",
        "  @[doc] _,\n",
        "  --| Missing an annotation\n",
        "  --| Still the same block\n",
        "  _\n",
        ")\n",
        "--| Trailing documentation\n",
    );
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let comments = unit
        .unattached_text(&parser.arena)
        .iter()
        .map(|warning| source[warning.range.clone()].trim_end().to_owned())
        .collect::<Vec<_>>();

    assert_eq!(
        comments,
        [
            "--| Separated by a blank line",
            "--| Interrupted by an ordinary comment",
            "--| Missing an annotation\n  --| Still the same block",
            "--| Trailing documentation",
        ]
    );
}

#[test]
fn documentation_annotation_accepts_an_explicit_empty_argument_list() {
    let source = "--| Empty argument list\n@[doc()] _";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let documentation = unit.documentation(&parser.arena, &parser.spans);
    let [site] = documentation.as_slice() else { panic!("expected one documentation attachment") };
    assert!(site.directive.meta.arguments.is_empty());
    assert_eq!(site.directive.comment.as_ref().unwrap().text.as_ref(), "Empty argument list");
}

#[test]
fn source_unit_decodes_literal_splices_with_attached_text() {
    let source = "--| First line\n--| Second line\n@[literal] _";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let literals = unit.literals(&parser.arena, &parser.spans).unwrap();
    let [site] = literals.as_slice() else { panic!("expected exactly one literal splice") };
    assert_eq!(site.directive.text.text.as_ref(), "First line\nSecond line");
    assert!(matches!(parser.arena.terms[&site.payload], Term::Hole(Hole)));
    assert!(
        unit.unattached_text(&parser.arena).is_empty(),
        "a text block attached to `@[literal]` must not warn"
    );
}

#[test]
fn source_unit_rejects_literal_splices_without_an_attached_text_block() {
    let source = "@[literal] _";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    assert!(matches!(
        unit.literals(&parser.arena, &parser.spans),
        Err(LiteralDirectiveError::MissingText { .. })
    ));
}

#[test]
fn source_unit_rejects_literal_on_a_non_hole_term() {
    let source = "--| Text\n@[literal] 1";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    assert!(matches!(
        unit.literals(&parser.arena, &parser.spans),
        Err(LiteralDirectiveError::PayloadNotHole { .. })
    ));
}

#[test]
fn source_unit_rejects_literal_metadata_arguments() {
    let source = "--| Text\n@[literal(extra)] _";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    assert!(matches!(
        unit.literals(&parser.arena, &parser.spans),
        Err(LiteralDirectiveError::Invalid { .. })
    ));
}

#[test]
fn source_unit_decodes_relative_and_absolute_imports() {
    // `@(import(...))` is sugar for `@[import(...)] _`: both spellings decode
    // to the same import targets.
    for source in [
        r#"(@[import("../library.zy")] _, @[import("/opt/zydeco/core.zy")] _)"#,
        r#"(@(import("../library.zy")), @(import("/opt/zydeco/core.zy")))"#,
    ] {
        let mut parser = Parser::new();
        let unit = parser::SourceUnitParser::new()
            .parse(source, &mut parser, lexer::Lexer::new(source))
            .unwrap();

        let imports = unit.imports(&parser.arena, &parser.spans).unwrap();
        let targets = imports.iter().map(|site| &site.directive.target).collect::<Vec<_>>();

        assert_eq!(
            targets,
            [
                &ImportTarget::Path(std::path::PathBuf::from("../library.zy")),
                &ImportTarget::Path(std::path::PathBuf::from("/opt/zydeco/core.zy")),
            ],
            "spelling `{source}` decoded to different import targets"
        );
    }
}

#[test]
fn source_unit_decodes_a_numbered_input_import() {
    let source = "@[import(7)] _";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let imports = unit.imports(&parser.arena, &parser.spans).unwrap();
    let [site] = imports.as_slice() else { panic!("expected one import") };

    assert_eq!(site.directive.target, ImportTarget::Input(SourceNumber::new(7).unwrap()));
}

#[test]
fn source_unit_rejects_import_without_one_supported_target() {
    enum ExpectedImportError {
        TargetArity(usize),
        UnsupportedTarget,
        EmptyPath,
        NonPositiveInput,
    }

    let cases = [
        ("@[import] _", ExpectedImportError::TargetArity(0)),
        ("@[import(path)] _", ExpectedImportError::UnsupportedTarget),
        (r#"@[import("one.zy","two.zy")] _"#, ExpectedImportError::TargetArity(2)),
        (r#"@[import("")] _"#, ExpectedImportError::EmptyPath),
        ("@[import(0)] _", ExpectedImportError::NonPositiveInput),
        ("@[import(-1)] _", ExpectedImportError::NonPositiveInput),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let mut parser = Parser::new();
        let unit = parser::SourceUnitParser::new()
            .parse(source, &mut parser, lexer::Lexer::new(source))
            .unwrap();
        let error = unit.imports(&parser.arena, &parser.spans).unwrap_err();

        match (error, expected) {
            | (
                ImportDirectiveError::TargetArity { found, .. },
                ExpectedImportError::TargetArity(expected),
            ) => assert_eq!(found, expected),
            | (
                ImportDirectiveError::UnsupportedTarget { .. },
                ExpectedImportError::UnsupportedTarget,
            ) => {}
            | (ImportDirectiveError::EmptyPath { .. }, ExpectedImportError::EmptyPath) => {}
            | (
                ImportDirectiveError::NonPositiveInput { .. },
                ExpectedImportError::NonPositiveInput,
            ) => {}
            | (error, _) => panic!("unexpected import error: {error}"),
        }
    });
}

#[test]
fn source_unit_rejects_import_on_a_non_hole_term() {
    let source = r#"@[import("library.zy")] value"#;
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    assert!(matches!(
        unit.imports(&parser.arena, &parser.spans),
        Err(ImportDirectiveError::PayloadNotHole { .. })
    ));
}

#[test]
fn source_unit_decodes_builtin_operation_roles_on_terms() {
    let source = "@[builtin(int64_add)] _";
    let mut parser = Parser::new();
    let unit = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let builtins = unit.builtins(&parser.arena, &parser.spans).unwrap();
    let [site] = builtins.as_slice() else { panic!("expected one Builtin role") };

    assert_eq!(
        site.directive.role,
        BuiltinRole::Value(BuiltinValueRole::Integer(IntegerType::Int64, IntegerOperation::Add,))
    );
    let BuiltinLocation::Term { payload, .. } = site.location else {
        panic!("expected a term annotation")
    };
    assert!(matches!(parser.arena.terms[&payload], Term::Hole(Hole)));
}

#[test]
fn source_unit_decodes_typed_intrinsic_splices() {
    [
        ("vtype", IntrinsicRole::VType),
        ("ctype", IntrinsicRole::CType),
        ("thk", IntrinsicRole::Thk),
        ("ret", IntrinsicRole::Ret),
        ("unit", IntrinsicRole::Unit),
        ("i8", IntrinsicRole::Primitive(zydeco_syntax::PrimitiveType::Integer(IntegerType::Int8))),
        (
            "f64",
            IntrinsicRole::Primitive(zydeco_syntax::PrimitiveType::Float(
                zydeco_syntax::FloatType::Float64,
            )),
        ),
        ("string", IntrinsicRole::Primitive(zydeco_syntax::PrimitiveType::String)),
    ]
    .into_iter()
    .for_each(|(name, expected)| {
        // `@(intrinsic(role))` is sugar for `@[intrinsic(role)] _`: both
        // spellings decode to the same splice with an implicit hole payload.
        for source in [format!("@(intrinsic({name}))"), format!("@[intrinsic({name})] _")] {
            let mut parser = Parser::new();
            let unit = parser::SourceUnitParser::new()
                .parse(&source, &mut parser, lexer::Lexer::new(&source))
                .unwrap();

            let intrinsics = unit.intrinsics(&parser.arena, &parser.spans).unwrap();
            let [site] = intrinsics.as_slice() else {
                panic!("expected one intrinsic splice in `{source}`")
            };
            assert_eq!(
                site.directive.role, expected,
                "spelling `{source}` decoded to a different role"
            );
            assert!(
                matches!(parser.arena.terms[&site.payload], Term::Hole(Hole)),
                "expected a hole payload for `{source}`"
            );
        }
    });
}

#[test]
fn source_unit_rejects_ambiguous_or_malformed_intrinsic_splices() {
    enum ExpectedIntrinsicError {
        RoleArity(usize),
        RoleNotIdentifier,
        UnknownRole,
        PayloadNotHole,
    }

    let cases = [
        ("@[intrinsic] _", ExpectedIntrinsicError::RoleArity(0)),
        ("@(intrinsic)", ExpectedIntrinsicError::RoleArity(0)),
        ("@[intrinsic(vtype,ctype)] _", ExpectedIntrinsicError::RoleArity(2)),
        ("@(intrinsic(vtype,ctype))", ExpectedIntrinsicError::RoleArity(2)),
        (r#"@[intrinsic("vtype")] _"#, ExpectedIntrinsicError::RoleNotIdentifier),
        (r#"@(intrinsic("vtype"))"#, ExpectedIntrinsicError::RoleNotIdentifier),
        ("@[intrinsic(monad)] _", ExpectedIntrinsicError::UnknownRole),
        ("@(intrinsic(monad))", ExpectedIntrinsicError::UnknownRole),
        ("@[intrinsic(unit)] Unit", ExpectedIntrinsicError::PayloadNotHole),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let mut parser = Parser::new();
        let unit = parser::SourceUnitParser::new()
            .parse(source, &mut parser, lexer::Lexer::new(source))
            .unwrap();
        let error = unit.intrinsics(&parser.arena, &parser.spans).unwrap_err();

        match (error, expected) {
            | (
                IntrinsicDirectiveError::Invalid {
                    source: IntrinsicMetaError::RoleArity { found },
                    ..
                },
                ExpectedIntrinsicError::RoleArity(expected),
            ) => assert_eq!(found, expected),
            | (
                IntrinsicDirectiveError::Invalid {
                    source: IntrinsicMetaError::RoleNotIdentifier,
                    ..
                },
                ExpectedIntrinsicError::RoleNotIdentifier,
            ) => {}
            | (
                IntrinsicDirectiveError::Invalid {
                    source: IntrinsicMetaError::UnknownRole(_), ..
                },
                ExpectedIntrinsicError::UnknownRole,
            ) => {}
            | (
                IntrinsicDirectiveError::PayloadNotHole { .. },
                ExpectedIntrinsicError::PayloadNotHole,
            ) => {}
            | (error, _) => panic!("unexpected intrinsic error: {error}"),
        }
    });
}

#[test]
fn source_unit_rejects_malformed_builtin_roles() {
    enum ExpectedBuiltinError {
        RoleArity(usize),
        RoleNotIdentifier,
        UnknownRole,
    }

    let cases = [
        ("@[builtin] _", ExpectedBuiltinError::RoleArity(0)),
        ("@[builtin(int,char)] _", ExpectedBuiltinError::RoleArity(2)),
        (r#"@[builtin("int")] _"#, ExpectedBuiltinError::RoleNotIdentifier),
        ("@[builtin(number)] _", ExpectedBuiltinError::UnknownRole),
        ("@[builtin(vtype)] _", ExpectedBuiltinError::UnknownRole),
        ("@[builtin(thk)] _", ExpectedBuiltinError::UnknownRole),
        ("@[builtin(monad)] _", ExpectedBuiltinError::UnknownRole),
    ];

    cases.into_iter().for_each(|(source, expected)| {
        let mut parser = Parser::new();
        let unit = parser::SourceUnitParser::new()
            .parse(source, &mut parser, lexer::Lexer::new(source))
            .unwrap();
        let error = unit.builtins(&parser.arena, &parser.spans).unwrap_err();

        let BuiltinDirectiveError::Invalid { source, .. } = error else {
            panic!("expected an invalid Builtin role")
        };
        match (source.as_ref(), expected) {
            | (
                BuiltinMetaError::RoleArity { found },
                ExpectedBuiltinError::RoleArity(expected),
            ) => assert_eq!(*found, expected),
            | (BuiltinMetaError::RoleNotIdentifier, ExpectedBuiltinError::RoleNotIdentifier)
            | (BuiltinMetaError::UnknownRole(_), ExpectedBuiltinError::UnknownRole) => {}
            | (error, _) => panic!("unexpected Builtin error: {error}"),
        }
    });
}

#[test]
fn source_unit_wraps_exactly_one_complete_term() {
    let source = "begin let value = 1 that value end";
    let mut parser = Parser::new();
    let SourceUnit { root } = parser::SourceUnitParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    assert!(matches!(parser.arena.terms[&root], Term::Block(_)));
}

#[test]
fn parses_uniform_term_composition_forms() {
    let source = "begin let answer = seed that param seed that ret answer end";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Block(Block(body)) = &parser.arena.terms[&term] else {
        panic!("expected a context-forming block")
    };
    let Term::ContextBind(ContextBind {
        mode: DefinitionMode::Transparent,
        placement: Placement::That,
        tail,
        ..
    }) = &parser.arena.terms[body]
    else {
        panic!("expected a mobile transparent definition")
    };
    let Term::Param(Param { placement: Placement::That, tail, .. }) = &parser.arena.terms[tail]
    else {
        panic!("expected a mobile parameter")
    };
    assert!(matches!(parser.arena.terms[tail], Term::Ret(_)));

    let nominal = "def Hidden = Int64 in Hidden";
    let nominal = parser::SingleTermParser::new()
        .parse(nominal, &mut parser, lexer::Lexer::new(nominal))
        .unwrap();
    assert!(matches!(
        parser.arena.terms[&nominal],
        Term::ContextBind(ContextBind {
            mode: DefinitionMode::Nominal,
            placement: Placement::In,
            ..
        })
    ));
}

#[test]
fn parses_value_pi_abstractions_and_bindings() {
    let pi_source = "val pi (A : VType) (value : A) . A";
    let mut parser = Parser::new();
    let pi = parser::SingleTermParser::new()
        .parse(pi_source, &mut parser, lexer::Lexer::new(pi_source))
        .unwrap();
    assert!(matches!(parser.arena.terms[&pi], Term::ValPi(_)));

    let abstraction_source = "val (A : VType) (value : A) => value";
    let abstraction = parser::SingleTermParser::new()
        .parse(abstraction_source, &mut parser, lexer::Lexer::new(abstraction_source))
        .unwrap();
    assert!(matches!(parser.arena.terms[&abstraction], Term::ValAbs(_)));

    let local_source = "let val identity (value : Int64) : Int64 = value that 1 |> identity";
    let mut parser = Parser::new();
    let local = parser::SingleTermParser::new()
        .parse(local_source, &mut parser, lexer::Lexer::new(local_source))
        .unwrap();
    let Term::ContextBind(ContextBind { binding, placement: Placement::That, tail, .. }) =
        &parser.arena.terms[&local]
    else {
        panic!("expected a mobile value-function binding")
    };
    assert_eq!(binding.flavor, BindingFlavor::Value);
    assert!(matches!(
        parser.arena.terms[tail],
        Term::Pipeline(Pipeline { direction: PipelineDirection::Forward, .. })
    ));
}

#[test]
fn parses_both_pipeline_spellings_and_view_patterns() {
    let mut parser = Parser::new();
    let forward = "pair |> first Int64 String";
    let forward = parser::SingleTermParser::new()
        .parse(forward, &mut parser, lexer::Lexer::new(forward))
        .unwrap();
    assert!(matches!(
        parser.arena.terms[&forward],
        Term::Pipeline(Pipeline { direction: PipelineDirection::Forward, .. })
    ));

    let backward = "first Int64 String <| pair";
    let backward = parser::SingleTermParser::new()
        .parse(backward, &mut parser, lexer::Lexer::new(backward))
        .unwrap();
    assert!(matches!(
        parser.arena.terms[&backward],
        Term::Pipeline(Pipeline { direction: PipelineDirection::Backward, .. })
    ));

    let pattern_source = "let first[Int64, String] ~> selected = pair in selected";
    let pattern_term = parser::SingleTermParser::new()
        .parse(pattern_source, &mut parser, lexer::Lexer::new(pattern_source))
        .unwrap();
    let Term::ContextBind(ContextBind { binding, .. }) = &parser.arena.terms[&pattern_term] else {
        panic!("expected a binding with a view pattern")
    };
    let Pattern::View(ViewPattern { function, .. }) = &parser.arena.pats[&binding.binder] else {
        panic!("expected a view pattern")
    };
    let Term::App(Appli(head)) = &parser.arena.terms[function] else {
        panic!("expected an instantiated view head")
    };
    assert_eq!(head.len(), 3);

    let rendered = pattern_term.ugly(&Formatter::new(&parser.arena));
    assert_eq!(rendered, pattern_source);
    let mut roundtrip = Parser::new();
    parser::SingleTermParser::new()
        .parse(&rendered, &mut roundtrip, lexer::Lexer::new(&rendered))
        .unwrap();
}

#[test]
fn parses_manifest_existential_with_a_punned_field_binder() {
    let source = "exists (= Counter as Int64 : VType) . Counter";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Exists(Exists { parameters, body }) = &parser.arena.terms[&term] else {
        panic!("expected a manifest existential")
    };
    let [ExistentialParameter { annotations, binder }] = parameters.as_slice() else {
        panic!("expected one manifest parameter")
    };
    assert!(annotations.is_empty());
    let Pattern::Named(Named(field, payload)) = &parser.arena.pats[binder] else {
        panic!("expected a punned named binder")
    };
    let Pattern::Ann(Ann { tm: manifest, ty: kind }) = &parser.arena.pats[payload] else {
        panic!("expected the classifier to annotate the transparent payload")
    };
    let Pattern::Manifest(ManifestPattern { binder, definition }) = &parser.arena.pats[manifest]
    else {
        panic!("expected a transparent payload binder")
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
    assert_eq!(definition.plain(), "Int64");
    assert_eq!(kind.plain(), "VType");
    assert_eq!(body.plain(), "Counter");

    let rendered = term.ugly(&Formatter::new(&parser.arena));
    assert_eq!(rendered, "exists (#Counter = ((Counter as Int64) : VType)) . Counter");
    let mut roundtrip = Parser::new();
    parser::SingleTermParser::new()
        .parse(&rendered, &mut roundtrip, lexer::Lexer::new(&rendered))
        .unwrap();
}

#[test]
fn parses_manifest_existential_inside_an_explicit_named_pattern() {
    let source = "exists (#Counter = ((Representation as Int64) : VType)) . Representation";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Exists(Exists { parameters, .. }) = &parser.arena.terms[&term] else {
        panic!("expected a manifest existential")
    };
    let [ExistentialParameter { binder, .. }] = parameters.as_slice() else {
        panic!("expected one manifest parameter")
    };
    let Pattern::Named(Named(field, payload)) = &parser.arena.pats[binder] else {
        panic!("expected an explicitly named binder")
    };
    let Pattern::Paren(Paren(payload)) = &parser.arena.pats[payload] else {
        panic!("expected the explicit grouping around the annotated payload")
    };
    let [payload] = payload.as_slice() else { panic!("expected one grouped payload") };
    let Pattern::Ann(Ann { tm: manifest, ty }) = &parser.arena.pats[payload] else {
        panic!("expected an annotated named payload")
    };
    let Pattern::Manifest(ManifestPattern { binder, definition }) = &parser.arena.pats[manifest]
    else {
        panic!("expected the transparent binder inside the named pattern")
    };
    let Pattern::Var(binder) = &parser.arena.pats[binder] else {
        panic!("expected a variable binder")
    };
    let Term::Var(definition) = &parser.arena.terms[definition] else {
        panic!("expected a transparent definition")
    };
    let Term::Var(ty) = &parser.arena.terms[ty] else { panic!("expected a payload classifier") };

    assert_eq!(field.plain(), "Counter");
    assert_eq!(parser.arena.defs[binder].plain(), "Representation");
    assert_eq!(definition.plain(), "Int64");
    assert_eq!(ty.plain(), "VType");
}

#[test]
fn parses_manifest_existential_with_an_inferred_classifier() {
    let source = "exists (VType as @[intrinsic(vtype)] _) . VType";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Exists(Exists { parameters, body }) = &parser.arena.terms[&term] else {
        panic!("expected a manifest existential")
    };
    let [ExistentialParameter { annotations, binder }] = parameters.as_slice() else {
        panic!("expected one manifest parameter")
    };
    assert!(annotations.is_empty());
    let Pattern::Manifest(ManifestPattern { binder, definition }) = &parser.arena.pats[binder]
    else {
        panic!("expected a transparent binder without a classifier")
    };
    let Pattern::Var(binder) = &parser.arena.pats[binder] else {
        panic!("expected an ordinary kind-variable binder")
    };
    let Term::Meta(MetaT(meta, payload)) = &parser.arena.terms[definition] else {
        panic!("expected the intrinsic definition to retain its metadata")
    };
    let Term::Hole(_) = parser.arena.terms[payload] else {
        panic!("expected the intrinsic metadata to annotate a hole")
    };
    let Term::Var(body) = &parser.arena.terms[body] else {
        panic!("expected the package body to use the bound kind")
    };

    assert_eq!(parser.arena.defs[binder].plain(), "VType");
    assert_eq!(
        meta.specialize::<IntrinsicMeta>().unwrap().map(|meta| meta.role),
        Some(IntrinsicRole::VType)
    );
    assert_eq!(body.plain(), "VType");
}

#[test]
fn parses_interleaved_abstract_and_manifest_existential_parameters() {
    let source = "exists (X : VType) (Y as X : VType) (value : Y) . X";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Exists(Exists { parameters, body }) = &parser.arena.terms[&term] else {
        panic!("expected an existential telescope")
    };
    let [abstract_type, manifest_type, value] = parameters.as_slice() else {
        panic!("expected three existential parameters")
    };
    assert!(matches!(parser.arena.pats[&abstract_type.binder], Pattern::Ann(_)));
    let Pattern::Ann(Ann { tm: manifest, .. }) = &parser.arena.pats[&manifest_type.binder] else {
        panic!("expected the manifest parameter to carry its classifier")
    };
    assert!(matches!(parser.arena.pats[manifest], Pattern::Manifest(_)));
    assert!(matches!(parser.arena.pats[&value.binder], Pattern::Ann(_)));
    let Term::Var(body) = &parser.arena.terms[body] else {
        panic!("expected the telescope body to be a type variable")
    };
    assert_eq!(body.plain(), "X");
}

#[test]
fn parses_pack_introduction_with_a_manifest_telescope() {
    let source = "pack (X as Int64 : VType) (Y as Char) where (0 : X), 'a' : Y, end";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Pack(Pack { parameters, body }) = &parser.arena.terms[&term] else {
        panic!("expected a package introduction")
    };
    let [classified, inferred] = parameters.as_slice() else {
        panic!("expected two manifest parameters")
    };
    assert!(classified.evidence.is_none() && inferred.evidence.is_none());
    let Pattern::Ann(Ann { tm: manifest, ty: kind }) =
        &parser.arena.pats[&classified.parameter.binder]
    else {
        panic!("expected the first parameter to carry its classifier")
    };
    assert!(matches!(parser.arena.pats[manifest], Pattern::Manifest(_)));
    let Term::Var(kind) = &parser.arena.terms[kind] else { panic!("expected a classifier") };
    let Pattern::Manifest(ManifestPattern { binder, .. }) =
        &parser.arena.pats[&inferred.parameter.binder]
    else {
        panic!("expected the second parameter to infer its classifier")
    };
    let Pattern::Var(inner) = &parser.arena.pats[binder] else {
        panic!("expected the inferred parameter to bind a type variable")
    };
    let Term::Paren(Paren(components)) = &parser.arena.terms[body] else {
        panic!("expected the payload to retain its comma sequence")
    };
    assert_eq!(components.len(), 2);

    assert_eq!(kind.plain(), "VType");
    assert_eq!(parser.arena.defs[inner].plain(), "Y");

    let rendered = term.ugly(&Formatter::new(&parser.arena));
    let mut roundtrip = Parser::new();
    parser::SingleTermParser::new()
        .parse(&rendered, &mut roundtrip, lexer::Lexer::new(&rendered))
        .expect("rendered package introductions must reparse");
}

#[test]
fn rejects_an_empty_pack_payload() {
    let source = "pack (X as Int64 : VType) where end";
    let mut parser = Parser::new();
    assert!(
        parser::SingleTermParser::new()
            .parse(source, &mut parser, lexer::Lexer::new(source))
            .is_err(),
        "a package payload must list at least one component"
    );
}

#[test]
fn parses_pack_introduction_with_sealed_evidence() {
    let source = "pack (= Bool : VType) is Bool (#Flag = Flag) is (List Bool) where 0 end";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Pack(Pack { parameters, .. }) = &parser.arena.terms[&term] else {
        panic!("expected a package introduction")
    };
    let [punned, named] = parameters.as_slice() else { panic!("expected two sealed parameters") };
    let Pattern::Named(Named(field, _)) = &parser.arena.pats[&punned.parameter.binder] else {
        panic!("expected the first parameter to pun its binder")
    };
    assert_eq!(field.plain(), "Bool");
    let Term::Var(evidence) = &parser.arena.terms[&punned.evidence.expect("sealed evidence")]
    else {
        panic!("expected atomic evidence")
    };
    assert_eq!(evidence.plain(), "Bool");
    let Term::Paren(Paren(components)) =
        &parser.arena.terms[&named.evidence.expect("sealed evidence")]
    else {
        panic!("expected parenthesized application evidence")
    };
    let Term::App(_) = &parser.arena.terms[&components[0]] else {
        panic!("expected an application inside the evidence parentheses")
    };

    let rendered = term.ugly(&Formatter::new(&parser.arena));
    let mut roundtrip = Parser::new();
    parser::SingleTermParser::new()
        .parse(&rendered, &mut roundtrip, lexer::Lexer::new(&rendered))
        .expect("rendered sealed parameters must reparse");
}

#[test]
fn parses_named_term_fields() {
    let source = "(#x = 1, #y = 2)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
            let Term::Lit(Literal::Integer(value)) = &parser.arena.terms[body] else {
                panic!("expected an integer payload")
            };
            (name.plain(), *value)
        })
        .collect::<Vec<_>>();

    assert_eq!(
        fields,
        vec![
            ("x".to_string(), IntegerLiteral::Unresolved(1)),
            ("y".to_string(), IntegerLiteral::Unresolved(2)),
        ]
    );
}

#[test]
fn parses_comma_separated_named_terms_without_early_sorting() {
    let source = "(#x = Int64, #y = String)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
        vec![("x".to_string(), "Int64".to_string()), ("y".to_string(), "String".to_string()),]
    );
}

#[test]
fn parses_labeled_product_type() {
    let source = "(#x :: Int64) * (#y :: String)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Term::Prod(Prod(components)) = &parser.arena.terms[&term] else {
        panic!("expected a product type")
    };
    let [left, right] = components.as_slice() else { panic!("expected two product components") };
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
    assert_eq!(left_type.plain(), "Int64");
    assert_eq!(right_name.plain(), "y");
    assert_eq!(right_type.plain(), "String");
}

#[test]
fn parses_chained_labels_right_associatively() {
    let source = "(#outer :: #inner :: Int64)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    assert_eq!(payload.plain(), "Int64");
}

#[test]
fn annotation_binds_inside_a_named_classifier() {
    let source = "(#field :: A : K)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    let source = "(#outer = #inner :: Int64)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    assert_eq!(payload.plain(), "Int64");
}

#[test]
fn parentheses_classify_the_whole_named_introduction() {
    let source = "((#field = value) : (#field :: classifier))";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    let source = "(#name = 1 : _)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    assert!(matches!(
        parser.arena.terms[tm],
        Term::Lit(Literal::Integer(IntegerLiteral::Unresolved(1)))
    ));
    assert!(matches!(parser.arena.terms[ty], Term::Hole(Hole)));
}

#[test]
fn parses_punned_named_terms_and_payload_annotations() {
    let source = "(= left, middle, = right : Int64)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    assert_eq!(ty.plain(), "Int64");
}

#[test]
fn field_names_and_puns_accept_uppercase_variable_names() {
    let source = "(#Explicit = payload, = Inferred)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    let parsed =
        parser::SingleTermParser::new().parse(source, &mut parser, lexer::Lexer::new(source));

    assert!(parsed.is_err());
}

#[test]
fn parses_chained_named_terms() {
    let source = "(#outer = #inner = 1)";
    let mut parser = Parser::new();
    let term = parser::SingleTermParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    assert!(matches!(
        parser.arena.terms[body],
        Term::Lit(Literal::Integer(IntegerLiteral::Unresolved(1)))
    ));
}

#[test]
fn parses_named_pattern_fields() {
    let source = "(#x = left, #y = right)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
fn parses_semicolon_pattern_aliases_in_source_order() {
    let source = "(whole; first; second)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Pattern::Alias(Alias(patterns)) = &parser.arena.pats[&pattern] else {
        panic!("expected a pattern alias")
    };
    let names = patterns
        .iter()
        .map(|pattern| {
            let Pattern::Var(definition) = parser.arena.pats[pattern] else {
                panic!("expected a variable alias member")
            };
            parser.arena.defs[&definition].plain()
        })
        .collect::<Vec<_>>();

    assert_eq!(names, ["whole", "first", "second"]);
}

#[test]
fn parses_field_projection_patterns_as_alias_members() {
    let source = "(/x = left; /y = right; whole)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Pattern::Alias(Alias(patterns)) = &parser.arena.pats[&pattern] else {
        panic!("expected a pattern alias")
    };
    let fields = patterns
        .iter()
        .take(2)
        .map(|pattern| {
            let Pattern::Project(ProjectionPattern(field, payload)) = &parser.arena.pats[pattern]
            else {
                panic!("expected a field projection pattern")
            };
            let Pattern::Var(payload) = &parser.arena.pats[payload] else {
                panic!("expected a variable projection payload")
            };
            (field.plain(), parser.arena.defs[payload].plain())
        })
        .collect::<Vec<_>>();

    assert_eq!(
        fields,
        [("x".to_string(), "left".to_string()), ("y".to_string(), "right".to_string())]
    );
}

#[test]
fn parses_punned_field_projection_patterns_and_payload_annotations() {
    let source = "(/left : Int64; /Right; whole)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Pattern::Alias(Alias(patterns)) = &parser.arena.pats[&pattern] else {
        panic!("expected a pattern alias")
    };
    let patterns = patterns.iter().copied().collect::<Vec<_>>();
    let [left, right, _whole] = patterns.as_slice() else { panic!("expected three alias members") };
    let Pattern::Project(ProjectionPattern(left_name, left)) = &parser.arena.pats[left] else {
        panic!("expected a punned left projection")
    };
    let Pattern::Ann(Ann { tm: left, ty }) = &parser.arena.pats[left] else {
        panic!("expected the left payload to be annotated")
    };
    let Pattern::Var(left) = &parser.arena.pats[left] else {
        panic!("expected a generated left binder")
    };
    let Term::Var(ty) = &parser.arena.terms[ty] else { panic!("expected a variable annotation") };
    let Pattern::Project(ProjectionPattern(right_name, right)) = &parser.arena.pats[right] else {
        panic!("expected a punned right projection")
    };
    let Pattern::Var(right) = &parser.arena.pats[right] else {
        panic!("expected a generated right binder")
    };

    assert_eq!(left_name.plain(), "left");
    assert_eq!(parser.arena.defs[left].plain(), "left");
    assert_eq!(ty.plain(), "Int64");
    assert_eq!(right_name.plain(), "Right");
    assert_eq!(parser.arena.defs[right].plain(), "Right");
}

#[test]
fn parses_chained_field_projection_patterns_right_associatively() {
    let source = "(/outer = /inner = payload)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
        .unwrap();

    let Pattern::Paren(Paren(patterns)) = &parser.arena.pats[&pattern] else {
        panic!("expected a parenthesized projection pattern")
    };
    let [pattern] = patterns.as_slice() else { panic!("expected one pattern") };
    let Pattern::Project(ProjectionPattern(outer, inner)) = &parser.arena.pats[pattern] else {
        panic!("expected an outer projection pattern")
    };
    let Pattern::Project(ProjectionPattern(inner_name, payload)) = &parser.arena.pats[inner] else {
        panic!("expected an inner projection pattern")
    };

    assert_eq!(outer.plain(), "outer");
    assert_eq!(inner_name.plain(), "inner");
    assert!(matches!(parser.arena.pats[payload], Pattern::Var(_)));
}

#[test]
fn parses_named_pattern_payload_annotation() {
    let source = "(#name = payload : _)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    let source = "(= left : Int64, middle, = right)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
    assert_eq!(ty.plain(), "Int64");
    assert_eq!(parser.arena.defs[middle].plain(), "middle");
    assert_eq!(right_name.plain(), "right");
    assert_eq!(parser.arena.defs[right].plain(), "right");
}

#[test]
fn parses_chained_named_patterns() {
    let source = "(#outer = #inner = payload)";
    let mut parser = Parser::new();
    let pattern = parser::SinglePatternParser::new()
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
        .parse(source, &mut parser, lexer::Lexer::new(source))
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
