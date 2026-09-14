use super::*;
use crate::metadata::PackageRelationKind;
use crate::textual::{ImportDirectiveError, ImportTarget, StrictParser};

struct Fixture {
    parser: Parser,
    unit: SourceUnit,
}

impl Fixture {
    fn parse(source: &str) -> Self {
        let mut parser = Parser::new();
        let unit = StrictParser::source(source, &mut parser).unwrap();
        Self { parser, unit }
    }

    fn packages(&self) -> Result<Vec<PackageSite>, PackageDirectiveError> {
        self.unit.packages(&self.parser.arena, &self.parser.spans)
    }

    fn import(source: &str) -> Result<ImportTarget, ImportDirectiveError> {
        let fixture = Self::parse(source);
        Ok(fixture
            .unit
            .imports(&fixture.parser.arena, &fixture.parser.spans)?
            .remove(0)
            .directive
            .target)
    }
}

#[test]
fn tests_can_name_multiple_subjects_and_keep_each_source_span() {
    let source = r#"@[package(test(of("lib.zy", other)), documentation("docs.zy"))] ()"#;
    let packages = Fixture::parse(source).packages().unwrap();
    assert_eq!(packages[0].role, PackageRole::Test);
    for (relation, expected) in packages[0].relations.iter().zip([r#""lib.zy""#, "other"]) {
        assert_eq!(relation.kind, PackageRelationKind::TestOf);
        assert_eq!(relation.target.to_string(), expected);
        assert_eq!(&source[relation.info.range()], expected);
    }
    for source in ["@[package(test)] ()", "@[package(test())] ()"] {
        assert!(Fixture::parse(source).packages().unwrap()[0].relations.is_empty());
    }
}

#[test]
fn test_subjects_reject_wrong_placement_shape_and_duplicate_targets() {
    for role in ["test(of())", "test(of(\"one.zy\"), of(\"two.zy\"))", "test(\"one.zy\")"] {
        assert!(matches!(
            Fixture::parse(&format!("@[package({role})] ()")).packages(),
            Err(PackageDirectiveError::Annotation {
                source: PackageAnnotationError::TestOptions,
                ..
            })
        ));
    }
    let source = "@[package(test(of(1)))] ()";
    let error = Fixture::parse(source).packages().unwrap_err();
    assert!(matches!(
        &error,
        PackageDirectiveError::Annotation { source: PackageAnnotationError::Target(_), .. }
    ));
    assert_eq!(&source[error.span().range()], "1");
    for role in ["library", "test"] {
        assert!(matches!(
            Fixture::parse(&format!(r#"@[package({role}, of("lib.zy"))] ()"#)).packages(),
            Err(PackageDirectiveError::Annotation {
                source: PackageAnnotationError::OfPlacement,
                ..
            })
        ));
    }
    let source = r#"@[package(test(of("lib.zy", "lib.zy")))] ()"#;
    let error = Fixture::parse(source).packages().unwrap_err();
    let PackageDirectiveError::DuplicateRelation { span, first } = error else { panic!("{error}") };
    assert_ne!(span, first);
    assert_eq!(&source[span.range()], r#""lib.zy""#);
    let source = r#"@[package(test(of("lib.zy", "invalid.zy#")))] ()"#;
    let error = Fixture::parse(source).packages().unwrap_err();
    assert!(matches!(
        &error,
        PackageDirectiveError::Annotation { source: PackageAnnotationError::Target(_), .. }
    ));
    assert_eq!(&source[error.span().range()], r#""invalid.zy#""#);
}

#[test]
fn invalid_annotation_arguments_keep_their_precise_locations() {
    for (argument, expected, error) in [
        ("binary", "binary", PackageAnnotationError::Relation),
        (r#"code("lib.zy")"#, r#"code("lib.zy")"#, PackageAnnotationError::Code),
        (r#"of("lib.zy")"#, r#"of("lib.zy")"#, PackageAnnotationError::OfPlacement),
    ] {
        let source = format!("@[package(library, {argument})] ()");
        let PackageDirectiveError::Annotation { span, source: found } =
            Fixture::parse(&source).packages().unwrap_err()
        else {
            panic!("expected argument error")
        };
        assert_eq!(found, error);
        assert_eq!(&source[span.range()], expected);
    }
}

#[test]
fn discovery_is_file_level_ordered_and_validates_each_glob() {
    use crate::textual::DiscoveryDirectiveError;
    let source = r#"(@[discover(include("tests/*.zy", "examples/*.zy"), exclude("tests/broken.zy"), include("tests/broken.zy"))] @[package(library)] ())"#;
    let fixture = Fixture::parse(source);
    let rules = fixture.unit.discovery(&fixture.parser.arena, &fixture.parser.spans).unwrap();
    assert_eq!(
        rules
            .iter()
            .map(|rule| (rule.kind.to_string(), rule.pattern.to_string()))
            .collect::<Vec<_>>(),
        [
            ("include", "tests/*.zy"),
            ("include", "examples/*.zy"),
            ("exclude", "tests/broken.zy"),
            ("include", "tests/broken.zy")
        ]
        .map(|(kind, pattern)| (kind.into(), pattern.into()))
    );
    for rule in rules {
        assert_eq!(&source[rule.info.range()], format!("\"{}\"", rule.pattern));
    }
    for (source, expected) in [
        ("(#x = @[discover] ())", "file root"),
        ("@[discover] @[discover] ()", "duplicate"),
        ("@[discover(include())] ()", "at least one"),
        (r#"@[discover(include("tests/../*.zy"))] ()"#, "relative glob"),
    ] {
        let fixture = Fixture::parse(source);
        let error =
            fixture.unit.discovery(&fixture.parser.arena, &fixture.parser.spans).unwrap_err();
        assert!(error.to_string().contains(expected), "{error}");
        if let DiscoveryDirectiveError::Invalid {
            source: crate::metadata::DiscoveryAnnotationError::Pattern { .. },
            span,
        } = error
        {
            assert_eq!(&source[span.range()], r#""tests/../*.zy""#);
        }
    }
}

#[test]
fn metadata_names_register_exact_terms_and_roles_are_separate_from_relationships() {
    let source = r#"(@[package(test, name(z))] 1,
        #unrelated = @[package(library, test(smoke), documentation("docs.zy"), name(a))] 2)"#;
    let fixture = Fixture::parse(source);
    let packages = fixture.packages().unwrap();
    assert_eq!(
        packages.iter().map(|site| site.name.as_ref().unwrap().to_string()).collect::<Vec<_>>(),
        ["a", "z"]
    );
    let library = &packages[0];
    assert_eq!(library.role, PackageRole::Library(crate::metadata::LibraryRole::Source));
    let Term::Meta(MetaTerm(_, payload)) = fixture.parser.arena.terms[&library.term] else {
        panic!("exact annotated term")
    };
    assert_eq!(source[fixture.parser.spans[&payload.into()].range()].trim(), "2");
    assert_ne!(library.term, fixture.unit.root);
    assert_eq!(library.relations[0].kind, crate::metadata::PackageRelationKind::Test);
    assert_eq!(library.relations[0].target.to_string(), "smoke");
    assert!(
        matches!(&library.relations[1].kind, crate::metadata::PackageRelationKind::Custom(name) if name.to_string() == "documentation")
    );
    assert_eq!(&source[library.relations[0].info.range()], r#"test(smoke)"#);
}

#[test]
fn root_annotations_need_no_name_and_preserve_transparent_wrappers() {
    for source in ["@[package(library)] 1", "(@[doc] (@[package(library)] 1) : @(intrinsic(i64)))"]
    {
        let fixture = Fixture::parse(source);
        let packages = fixture.packages().unwrap();
        assert_eq!(packages.len(), 1);
        assert_eq!(packages[0].name, None);
        assert_eq!(packages[0].term, fixture.unit.root);
    }
    assert!(
        Fixture::parse("42").packages().unwrap().is_empty(),
        "implicit file packages need no synthetic annotation"
    );
}

#[test]
fn annotations_require_a_role_and_package_names_are_unique_across_roles() {
    for source in [
        "@[package] ()",
        r#"@[package(unknown)] ()"#,
        r#"@[package(library("old"))] ()"#,
        r#"@[package(library("old", "lib.zy"))] ()"#,
    ] {
        assert!(matches!(
            Fixture::parse(source).packages(),
            Err(PackageDirectiveError::Annotation {
                source: PackageAnnotationError::Role | PackageAnnotationError::LibraryAbi,
                ..
            })
        ));
    }
    assert!(matches!(
        Fixture::parse(r#"@[package(library, binary)] ()"#).packages(),
        Err(PackageDirectiveError::Annotation { source: PackageAnnotationError::Relation, .. })
    ));
    assert!(matches!(
        Fixture::parse(r#"(#same = @[package(library, name(same))] (), #different = @[package(test, name(same))] ())"#).packages(),
        Err(PackageDirectiveError::DuplicateName { name, span, first }) if name.to_string() == "same" && span != first
    ));
    assert!(matches!(
        Fixture::parse("let x = @[package(library)] 1 in x").packages(),
        Err(PackageDirectiveError::Unnamed { .. })
    ));
}

#[test]
fn names_are_optional_at_file_roots_and_required_only_for_nested_entries() {
    for source in [
        r#"@[package(library, name(api))] 1"#,
        r#"let x = @[package(library, name(api))] 1 in x"#,
        r#"(#field = @[package(library, name(api))] 1)"#,
    ] {
        let packages = Fixture::parse(source).packages().unwrap();
        assert_eq!(packages[0].name.as_ref().unwrap().to_string(), "api");
        assert!(packages[0].relations.is_empty(), "name is not a relationship");
    }
    for source in [r#"let x = @[package(library)] 1 in x"#, r#"(#field = @[package(library)] 1)"#] {
        assert!(matches!(
            Fixture::parse(source).packages(),
            Err(PackageDirectiveError::Unnamed { .. })
        ));
    }
}

#[test]
fn name_options_validate_spelling_shape_and_uniqueness_at_the_option_span() {
    for (option, expected) in [
        (r#"name("api")"#, PackageAnnotationError::NameShape),
        ("name()", PackageAnnotationError::NameShape),
        ("name(1)", PackageAnnotationError::NameShape),
        (r#"name("x", "y")"#, PackageAnnotationError::NameShape),
        (r#"name("")"#, PackageAnnotationError::NameShape),
        (r#"name("a/b")"#, PackageAnnotationError::NameShape),
    ] {
        let source = format!("@[package(library, {option})] 1");
        let PackageDirectiveError::Annotation { span, source: error } =
            Fixture::parse(&source).packages().unwrap_err()
        else {
            panic!("name error")
        };
        assert_eq!(error, expected);
        assert_eq!(&source[span.range()], option);
    }
    let source = r#"@[package(library, name(one), name(two))] 1"#;
    let PackageDirectiveError::Annotation { span, source: error } =
        Fixture::parse(source).packages().unwrap_err()
    else {
        panic!("duplicate name")
    };
    assert_eq!(error, PackageAnnotationError::DuplicateName);
    assert_eq!(&source[span.range()], r#"name(two)"#);
}

#[test]
fn malformed_relationships_and_redundant_code_declarations_are_rejected() {
    assert!(matches!(
        Fixture::parse(r#"@[package(library, code("lib.zy"))] ()"#).packages(),
        Err(PackageDirectiveError::Annotation { source: PackageAnnotationError::Code, .. })
    ));
    assert!(
        matches!(Fixture::parse(r#"@[package(library, test("tests.zy"), test("tests.zy"))] ()"#).packages(),
        Err(PackageDirectiveError::DuplicateRelation { span, first }) if span != first)
    );
    for target in [r#"test(1)"#, r#"test("")"#, r#"test("tests.zy#")"#, r##"test("#smoke")"##] {
        assert!(matches!(
            Fixture::parse(&format!("@[package(library, {target})] ()")).packages(),
            Err(PackageDirectiveError::Annotation {
                source: PackageAnnotationError::Target(_),
                ..
            })
        ));
    }
    for relation in [r#"test"#, r#"relation("test", package("tests.zy", "smoke"))"#] {
        assert!(matches!(
            Fixture::parse(&format!("@[package(library, {relation})] ()")).packages(),
            Err(PackageDirectiveError::Annotation { source: PackageAnnotationError::Relation, .. })
        ));
    }
}

#[test]
fn source_references_distinguish_catalog_names_from_explicit_whole_files() {
    use crate::metadata::{PackageName, SourceReference};
    for (written, expected) in [
        (r#""math.zy""#, SourceReference::Path("math.zy".into())),
        (r#""std""#, SourceReference::Path("std".into())),
        ("std", SourceReference::Package("std".parse().unwrap())),
        ("std/data", SourceReference::Package("std/data".parse().unwrap())),
        ("std/test/foo-bar", SourceReference::Package("std/test/foo-bar".parse().unwrap())),
    ] {
        let target = Fixture::import(&format!("@(import({written}))")).unwrap();
        assert_eq!(target, ImportTarget::Source(expected));
        assert_eq!(target.to_string(), written);
    }
    for path in ["math.zy", "lib/math.zy", "./std", "../std", "/std", "math.zyi", "math.zydeco"] {
        assert_eq!(path.parse::<SourceReference>().unwrap(), SourceReference::Path(path.into()));
    }
    for name in ["std", "std/data", "pkg/test-1", "_pkg"] {
        assert_eq!(
            name.parse::<SourceReference>().unwrap(),
            SourceReference::Package(name.parse().unwrap())
        );
        let source = format!("@[package(library, name({name}))] ()");
        assert_eq!(
            Fixture::parse(&source).packages().unwrap()[0].name,
            Some(name.parse().unwrap())
        );
    }
    for name in [
        "",
        "/",
        "std/",
        "/std",
        "std//data",
        "std/../data",
        "1std",
        "std/1",
        "_",
        "std/_",
        "std#data",
    ] {
        assert!(name.parse::<PackageName>().is_err(), "{name}");
    }
    assert!(matches!(Fixture::import("@(import(1))").unwrap(), ImportTarget::Input(_)));
}

#[test]
fn invalid_source_references_and_old_package_calls_are_rejected() {
    for address in ["#main", "math.zy#", "math.zy#../main", "math.zy#one#two", "a\0b"] {
        let source = format!("@(import({address:?}))");
        assert!(
            matches!(Fixture::import(&source), Err(ImportDirectiveError::InvalidSource { .. })),
            "{address:?}"
        );
    }
    assert!(matches!(
        Fixture::import(r#"@(import(""))"#),
        Err(ImportDirectiveError::EmptyPath { .. })
    ));
    assert!(matches!(
        Fixture::import(r#"@(import(package("math.zy", "main")))"#),
        Err(ImportDirectiveError::InvalidSource { .. })
    ));
    assert!(matches!(
        Fixture::import(r#"@[import("math.zy")] 1"#),
        Err(ImportDirectiveError::PayloadNotHole { .. })
    ));
}

#[test]
fn metadata_catalog_agrees_with_package_argument_and_source_validation() {
    use crate::metadata::{MetadataCatalog, MetadataValidationError};
    let annotation = |source: &str| {
        let fixture = Fixture::parse(source);
        let Term::Meta(MetaTerm(meta, _)) = fixture.parser.arena.terms[&fixture.unit.root] else {
            panic!("annotation")
        };
        fixture.parser.arena.semantic_meta(meta)
    };
    let valid = annotation(r#"@[package(library, test(smoke), documentation("docs.zy"))] ()"#);
    assert!(MetadataKind::Package.definition().validate_arguments(valid.arguments()).is_ok());
    let invalid = annotation(r#"@[package(library, test(1))] ()"#);
    assert!(matches!(
        MetadataKind::Package.definition().validate_arguments(invalid.arguments()),
        Err(MetadataValidationError::Package(PackageAnnotationError::Target(_)))
    ));
    let test =
        MetadataCatalog::package_options().iter().find(|option| option.name() == "test").unwrap();
    assert!(test.validate_arguments(&[Meta::Ident("smoke".into())]).is_ok());
    assert!(matches!(
        test.validate_arguments(&[Meta::Integer(1)]),
        Err(MetadataValidationError::ExpectedSource { .. })
    ));
    assert!(matches!(
        test.validate_arguments(&[Meta::String("tests.zy#".into())]),
        Err(MetadataValidationError::ExpectedSource { .. })
    ));
}
